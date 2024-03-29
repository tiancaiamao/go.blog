最近工作中遇到一个蛮有意思的 case，拿出来分享一下。

起因是这里一个 [issue](https://github.com/pingcap/tidb/issues/48411)，有个 error 报错没有返回给客户端，而是打 log 记录了下来。

```
defer terror.Call(rs.Close)
```

咋一看觉得，只要把 `rs.Close` 的 error 返回就能够解决了，于是修改试了一下，结果发现一个更意想不到的情况。

```
defer func() {
  err = rs.Close()
}
...
writeChunks(rs)
...
```

rs 是 ResultSet，我们会不停地调用 `ResultSet.Next()` 把里面的数据读出来，然后再 `ResultSet.Close()` 关闭这个 ResultSet。

新的问题出现在，当我们 writeChunk() 完成时，会给 mysql 客户端 writeEOF，表示数据包已经写完了。那么接下来在 Close() 的时候发生的错误，就再也没有机会返回给 mysql 客户端了。

ResultSet 的 Close() 里面会调用 finishStmt，它会有一些额外的清理工作，包括处理 statement 的缓存，以及如果有涉及语句的 autocommit 会将事务提交等。
**如果事务提交出现了 error，而这个 error 信息又没有返回客户端，客户端就会以为事务提交成功了。再后面查询，却读到到刚刚的数据，那么客户就会认为丢数据了**，这对于 tidb 这种数据库来说会是非常严重的问题。数据库怎么能丢数据呢？这一定是个误会。这个 bug 确实是这样的情况。

**如果我们回复 mysql packet 结束了，再发生错误，就没机会把客户传给客户端**。那么正确的处理逻辑顺序，应该是这样子：

```
for {
    data = rs.Next()
    writePacket(data)
}
err = rs.Close()
if err != nil {
  writeError()
} else {
  writeEOF()
}
```

也就是先把 result set 处理完，关闭 result set 之后，才是真正的结束。然后我就意识到：这是一个设计层面的问题导致的代码脆弱性。

- 为什么我们会犯这样的错误？

因为我们把 ResultSet 的 defer Close 跟数据 drain 过程拆开到不同的函数生命期了，导致 writeEOF 是随着 drain 数据的生命期结束而结束，而不是随着 ResultSet 最后的 Close 过程结束的。

- 为什么我们会使用 defer 这种写法呢？

因为 ResultSet 是一个 lazy 对象，它不是一个具体的 []byte 数据，需要不停的 Next() 才会吐出来 []byte，最终通过 Close 方法关闭掉。
ResultSet 背后的实现可以看作是绑定了许多 goroutine 和 channel，goroutine 在读数据和处理数据，结果往 channel 里面写，Next() 就是读取 channel 来得到 []byte。defer Close 写法是为了保证即使中间处理出现 error 了，也能够最终释放资源。

- 为什么 ResultSet 是一个 lazy 对象呢？

假如 ResultSet 不是一个 lazy 对象，就不需要绑定后面的资源和 Close 操作，假如它就是一个 []byte，就可以直接 writePacket(rs) 然后 writeEOF 结束了。
最初的设计将 ResultSet 做成 lazy，原因是为了 "省内存"。一条 sql 语句有可能是一个全表扫，那么 ResultSet 的结果将是巨大的，如果结果计算完毕后，全部缓存在 tidb 进程内部，再写到 mysql client，这个过程可能直接就撑爆 tidb 的内存了。所以 ResultSet 做成了一个 lazy 对象，只有在调用 Next() 的时候才会触发读取一批数据并返回到 mysql 客户端，通过不停地 Next() 整个过程"流式地"执行，只需要少量的内存就可以驱动整个传输过程。


把上面的几条 "为什么" 倒过来看就会发现，设计层面对代码健壮性的影响。出于内存考虑，ResultSet 需要做成 lzay 的，不停地调用 Next() 吐出数据；设计成 lazy 之后，会有后台绑定 goroutine 和 channel 等资源；为了确保 error 等异常情况下，不会出现资源泄漏，用了 defer Close 写法； 代码健壮性不好，defer 的 Close 如果发生了 error，该 error 无法返回客户端。

ResultSet 设计为 lazy 的这个 design，其实不是第一次造成问题了，像比如 cursor 的支持上面这个设计就造成了很大的麻烦。它天然就是一个很容易出错的东西，就好比异步代码天然地比同步代码容易出错。Next() 代表未完成的计算，如果 Next() 出错了，就要终止计算，这时会有一个 error。而如果 Close 的过程又出现了 error，就会在错误处理过程中再次 error，应该返回哪个 error?

霜爷([@crazycs520](https://github.com/crazycs520)) 本来提了一个这个 bug 修复的 PR，我看他的 PR 里面已经带了 unit test 了，我就不想自己再写 UT，直接把他的拿过来用，结果又有第二个意外的发现。

我发现在我本地 mysql client 手动验证我的 fix 是通过的，但是用霜爷的那个 UT 跑就会挂，我一度怀疑 go-sql-driver 那个包出 bug 了。因为在我这边的修复后的行为，是会返回一部分的数据包，再到最后返回 error 包，而霜爷那边的修复会首个消息直接返回 error 包，所以我怀疑会不会是 go-sql-driver 对这种正常读取了部分数据包，之后再出现的 error 包未能正确处理？

魔改打印了一些日志，把我们返回的数据跟 go-sql-driver 收到的数据比对之后，发现问题其实是我的测试代码的写法有点问题。我是这样写的：

```
var err error
rows := dbt.MustQuery("select * from t for update;")
for rows.Next() {
  var id int
	err = rows.Scan(&id)
	if err != nil {
		break
	}
}
require.Error(t, err)
```

而正确的写法，应该是这样子：

```
rows := dbt.MustQuery("select * from t for update;")
for rows.Next() {
    ...
}
err := rows.Err() 
require.Error(t, err)
```

`Scan()` 是不吐 error 的，应该是在 Next() 返回 false 之后，通过 iterator 的 Err() 方法去判断结束的原因是数据读完了，还是有 error 发生！

这又是一处设计对代码健壮性的影响，**这个 API 设计让使用者很容易犯错，忘记调用 Err() 方法，直接认为 iterator 完是正常结束的**。假设 Next() 返回的是 (bool, error) 则使用者不会犯这样的错误（这样对 for loop 写法的友好性可能就会差一些了）。如果出现这种写法，用户又可能会认为 tidb 丢数据了，其实用户自己写法不对，没有检查 error 而以为 sql 执行成功...
