sysbench 是一个常用的性能测试工具，比如用来测试数据库的性能。它是用 C 和 lua 写的，可以通过 lua 脚本进行扩展，如果想自己改一些配置参数或者改一下测试，都比较灵活。

不过 lua 我并不太习惯，平时还是写 Go 更顺手一些。于是问题来了，如果我用 Go 来实现一套 sysbench，如何达到同样的灵活性呢？ 毕竟 Go 是一门静态编译型的语言，能不能达到像脚本一样的灵活性。
这里主要思考的是：

- 提供怎样的使用形态
- 如果做到轻松扩展

比较确定的是，应该做成一个框架。库，是你在调它，而框架就是它在调你，使用者只在填空。这里特定场景，框架只需要暴露出来，怎么样 Prepare，怎么样 Run 以及 Cleanup。具体的细节，用多少 bench 线程，怎么管理连接的建立和释放，这些应试丢给框架。
然后是业务相关的，建表，测试的表的 schema，数据量的大小，这些部分应该做成由用户决定的。

于是我们可以做一下分类：

- 第一类是完全静态的，也就是框架写死的部分。特点是逻辑比较固定。
- 第二类是配置的参数，半动态的。也就是代码确定了，但是细节的值可变。
- 第三类是完全动态的，跑什么样的代码不确定。

静态配置的比如这么写：

```
Conn : ConnConfig {
     User: "root",
     Host: "127.0.0.1",
     Port: 3306,
     DB: "test",
}
```

**代码确定的，参数可配置，这很好做...框架是确定的，执行什么代码可配置，这就有点技巧性了。**

解决方式是使用 interface。

```
type RunConfig struct {
     WorkerCount int
     Task TaskRunner
}
```

注意，这个结构体里面的 Task 字段，它是一个 TaskRunner 接口：

```
type TaskRunner interface {
     CreateTable()
     InsertData()
     Query()
}
```

这样我们可以有不同的实现，比如 UpdateRange，SelectRandom，PointGet 等等等，实现不同的 TaskRunner。配置不同，对应的就可以跑不同的测试了。结构体本身也是可以存参数的，一个结构体，就是一个闭包。

比如这样子：

```
Task: SelectRandom {
      Isolation: SI,
      Range: RandomRange{},
}
```

或者这样子：

```
Task: UpdateRange {
      Count: 30,
      Conflict: 50,
}
```

对，对象是穷人的闭包。只要它们都是实现了 TaskRunner 接口的。

当我们把所有“配置”组合到一起的时候，可以得到一个更完整的例子：

```
conf := &Config {
     Conn: ConnConfig{
          User: "xxx",
          Host: "127.0.0.1",
          Port: 3306,
          DB: "test",
     },
     Prepare: DefaultPrepare(),
     Run: RunConfig {
          WorkerCount: 4,
          Task: SelectRandom {
                Isolation: SI,
                Range: RandomRange{},
          }
     },
     CleanUp: DefaultCleanUp(),
}
```

接下来我们要做成配置文件么？比如说

```
sysbench -config config.json
```

不不不！我想的是，做成一个 DSL！这个代码本身，既是配置，也描述了怎么运行了。Go 语言里面有 go test 命令，可以直接跑测试的。最后这样子会更酸爽：

```
package test

import . "github.com/tiancaiamao/sysbench"

func TestT(t *testing.T) {
	conf := &Config {
		Conn: ConnConfig{
			User: "xxx",
			Host: "127.0.0.1",
			Port: 3306,
			DB: "test",
		},
		Prepare: DefaultPrepare(),
		Run: RunConfig {
			WorkerCount: 4,
			Task: SelectRandom {
				Isolation: SI,
				Range: RandomRange{},
			}
		},
		CleanUp: DefaultCleanUp(),
	}
	RunTest(conf)
}
```

如果不想写配置，最简单的是全部提供默认值，这样就可以

```
RunTest(DefaultConfig())
```

自己定义不同的测试，可以修改一部分，比如：

```
package test

import . "github.com/tiancaiamao/sysbench"

func TestT(t *testing.T) {
	conf := DefaultConfig()
	conf.Run.Task = UpdateRange {}
	RunTest(conf)
}
```

以上示例都只是想法。坑挖完了，还没填...
