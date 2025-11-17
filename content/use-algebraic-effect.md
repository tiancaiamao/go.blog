最近的几篇博客都是关于 algebraic effect 的，主要是我发现这东西像发现宝了。它可以像 `call/cc` 一样强大，而又不至于太过失控。基于 algebraic effect 可以实现好多种其它的 feature，比如 async/await，异常，协程等等。
这些都属于具体应用场景了，也就是说只要有了 algebraic effect 这一个 feature，其它的 feature 都可以基于它去实现。

看两个具体的应用，一个是实现协程，另一个是实现异步IO。

协程的实现提供 spawn yield 和 run 方法。其中 spawn 用于新建一个协程；yield 用于让出当前协程的上下文；run 是入口函数，它接受一个待执行的 main 函数，并且函数里面是可以抛出 effect 的。
具体的实现，run 会处理 Spawn 和 Yield 两种 effect：

```
(defun run (main)
  (handler (lambda (_)
	     (if (empty?) "done"
	       (let task (dequeue) (task ()))))
	   ['Spawn (lambda (fn k)
		     (begin
		      (enqueue k)
		      (run fn)))
	   'Yield (lambda (_ cc)
		    (begin
		     (enqueue cc)
		     (let task (dequeue)
			  (task ()))))]
	   main))
```

然后还有一个运行队列的概念，`enqueue` 和 `dequeue` 从运行队列里面拿一个任务出来执行。

spawn 一个新的协程，就是 yield 一个 Spawn 的 effect：

```
(yield _ (eff 'Spawn coroutine)
	;; more work to do...)
```

当 run 函数处理 Spawn 的 effect 的时候，会将当前的上下文保存，放到运行队列，然后去执行 spawn 的 coroutine 函数。注意这里，调用的是 `(run fn)` 而不直接执行 `fn` 函数，是因为在执行的 coroutine 函数里面可能还会有 spawn 和 yield 的操作，需要支持相应的 effect handle。

yield 的实现是，yield 一个叫 Yield 的 effect:

```
(yield _ (eff 'Yield ())
	;; ... after return from yield ...)
```

在 run 函数里面处理 Yield effect 的时候，会将当前的连续放入运行队列，然后从运行队列取出下一个任务执行。

这样子就实现了协程的概念了，简单的 [demo](https://github.com/tiancaiamao/cora/blob/cb0d871339a60cfb2376cfddd88f5d4fc480162a/lib/coroutine.cora) 代码。

cora 的 yield 是[用宏实现的](/algebraic-effect2.md)，是一个语法糖。它只能放在函数的"尾调用"的位置，比如说，这样子是不行的：

```
(begin 1 (yield _ 42 ...) 2 3)
```

只能放在最后:

```
(begin 1 (yield _ 42 (begin ... 2 3)))
```

还有写法也只能是

```
(yield v XXX
	;; do with v)
```

而不能是

```
(let v (yield XXX)
	;; do with v)
```

以上面协程的，接下来看异步IO。

异步IO 的关链点是，在会 block 的调用上，把当前的协程让出去，然后等调用 ready 后再执行，实现上层阻塞，下层不阻塞的效果。
这样编程的心智负担是小很多的，比回调那种要靠谱。不过，复杂度是从使用者转移到实现者身上了。

在 algebraic effect 机制之上实现异步 IO 并不复杂。send 和 recv 不再是阻塞地调用系统函数，而是抛出 Send Recv 这样子的 effect。

```
(yield v (eff 'Send [fd buf pos len mode])
	...)
(yield v (eff 'Recv [fd buf pos len mode])
	...)
```

对于比如 `Recv` effect 的 handler，如果 fd 是可读的，则直接读取，否则，放到一个 block 队列里面去：

```
(func recv-handler
	[fd buf pos ...] k => (if (read-to-read fd)
	                          (k (os-recv fd buf pos len mode))
							  (begin
								  (enqueue fd ['Blocked eff k])
								  (schedule))))
```

schedule 里面，优先判断可运行队列里面，是否有任务可以运行，然后是 block 队列，执行 poll 的操作，等有 fd 收到信号以后，唤醒对应的因 IO 而 blocked 的任务。

```
(defun schedule ()
	(if (not-empty? (running-queue))
		(let task (pop (running-queue))
			(task ()))
		(if (empty? (blocked-queue))
			() ;; no more task
			(begin fds (collect-blocked-fds)
				(let ready-fds (os-poll fds)
					(match (find blocked-queue ready-fds)
						[Blocked [Recv fd buf ...] k] => (k (os-recv fd ...))
						[Blocked [Send fd buf ...] k] => (k (os-send fd ...))
						...)
					(remove ready-fds (blocked-queue)))
				(schedule)))))
```

如果没有可执行的任务，schedule 会在 poll 那里 block 往，等待 IO 就绪事件。大概的思路就是这样子的。

如果有了协程，有了异步 IO，接下来还可以考虑 channel 消息通讯之类的，这些都是可以实现成库的形式。相比 Go 语言之类的在语言中内置这些基础设施，库的形式提供会有什么优势呢？灵活性会更高一点。
比如 Go 的 goroutine 的调度是最基础的没有优先级概念的，而如果自己实现一套，就可以自定义调度的策略。而这些都不会对语言有侵入，只需要建立在最基础的 algebraic effect 功能之上...而 algebraic effect 也是可以用库实现的，感觉非常期待啊！
