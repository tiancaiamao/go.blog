我们有个内部集群跑着一个模拟客户流量的测试，前一阵子值班同事反馈说集群状态不太对，于是我登上去查了一下。结果发现客户端对 prepare 的使用姿势很有问题。

正常情况下，数据库执行一条 SQL 语句，要将文本转成 AST，然后再将 AST 做成 Plan(查询计划)，再执行 Plan。在高并发的时候，这些从文本到查询计划的过程还是蛮耗资源的。于是，比如在 MySQL 里面，就有 prepare 协议。第一步先发送一个 prepare 操作，生成一个 prepared 的 statement:


```
prepare stmt from 'select * from t where id = ?'
```

这里面有一个 ? 是参数，先空着。再接下来，就可以直接指定参数并使用 stmt 了:

```
set @x = 42;
execute stmt using @x;
```

只要 stmt 不释放，就可以多次的被重重使用。这样可以绕开每次发送 SQL 文本 parse 和生成查询计划的开销。

我发现我们内部测试集群里面，客户端执行的骚操作是，prepare 一次，execute 一次，立即释放掉，每条 SQL 都这么干。这太蠢了！相比于直接走 query 协议，不仅没有省掉生成 ast 和做 plan 开销，还额外引入了好几次网络的 roundtrip。然后我就去研究一下，测试程序是怎么写成这样的。

测试代码看起来没注意到异常，它是用的 sql.DB 或者 sql.Tx 这样调用：

```
db.QueryRow("select xxx from t where user_id = ?", arg)
```

所以那就是库的锅咯？[标准库的实现里面接口](https://golang.org/pkg/database/sql/#DB.Query)是这样的：

```
func (db *DB) QueryRow(query string, args ...interface{}) *Row
```

具体的代码实现会先尝试调用 `ctxDriverQuery`，如果返回了 `driver.ErrSkip`，就会用 prepare, execute 再 close 的方式了。
然后看一下 `go-sql-driver/mysql` 包：

```
        if len(args) != 0 {
                if !mc.cfg.InterpolateParams {
                        return nil, driver.ErrSkip
                }
                // try client-side prepare to reduce roundtrip
                prepared, err := mc.interpolateParams(query, args)
                if err != nil {
                        return nil, err
                }
                query = prepared
        }
		...
```

对于 query 函数传了参数的情况，如果没有配置 InterpolateParams，就会返回 `driver.ErrSkip`。(如果配置了 InterpolateParams，会把参数转成文本后使用 query 协议)

这个设计太蠢了，用户本以为用 `Query("select ...?", args ...)` 这种是 prepare 协议，可以提升性能，但实际是性能反而不如不带参数的写法。

结论：`QueryRow(query string, args ...interface{})` 带可变参数调用的时候对 prepare 的使用姿势有问题，它是 prepare 一次 execute 一次的。

标准库其实有另一个 `Prepare` [函数](https://pkg.go.dev/database/sql#DB.Prepare)，正确的使用 prepare 协议应该使用那个方法。
