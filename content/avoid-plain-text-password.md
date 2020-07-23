认证过程，不应该是客户端发送密码到服务端，然后服务端检查密码是否正确。因为，

* 密码绝不应该明文存储。
* 密码绝不应该明文在网络传输。

这里有一个简单的算法处理。客户端存 password，然后在服务端存 hash(password)。

认证的时候，由服务端发送一个 seed 给客户端。

然后客户端那边计算出 hash(seed, hash(password))，把这个结果返回服务端。

服务端这边比对一下结果，就 OK 了。


可以看到，存储方面，服务端是没有存储明文的。传输方面，也没有明文的密码在网络上传输。
不过这个简单算法里有个 hash 碰撞问题，因为不同的 key 在 hash 之后可能得到相同的值，有可能出现中间的密码用错了，但最后还是认证成功。

然后说一下混淆，利用 xor 两次之后会回到原来的值。

```
A xor B = C
A xor C = B
```

如果客户端和服务端之间约定一个魔数 A，客户端发送 A xor B，那么原始数据 B 混郩一次，变成了 C 在网络发送。服务端收到 C 以后，用魔数 A 做一个 xor 可以恢复出混淆前的数据 B。
这个魔数 A 选啥呢？ 其实不需要是一个特定的数据，只要是客户端和服务端都知道的数据就行。

最后看一下 `mysql_native_password` 的实现模式：

```
// The new authentication is performed in following manner:
//   SERVER:  public_seed=create_random_string()
//            send(public_seed)
//   CLIENT:  recv(public_seed)
//            hash_stage1=sha1("password")
//            hash_stage2=sha1(hash_stage1)
//            reply=xor(hash_stage1, sha1(public_seed,hash_stage2)
//            // this three steps are done in scramble()
//            send(reply)
//   SERVER:  recv(reply)
//            hash_stage1=xor(reply, sha1(public_seed,hash_stage2))
//            candidate_hash2=sha1(hash_stage1)
//            check(candidate_hash2==hash_stage2)
//            // this three steps are done in check_scramble()
```

其实道理是一样的，其中 hash 方法选用的是 sha1(sha1(password))，然后魔数 A 选的是 sha1(seed, hash(password))。
