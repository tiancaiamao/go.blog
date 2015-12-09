前几天的时候，去试玩了一把lua。试着写一个异步的网络库练手。用来写echo服务端大概是这个样子：

    local event = require("event")
    local acceptor = require("acceptor")
    function on_new_client(conn)
    event:add(conn)
    conn:recv(function (p, err)
        conn:send(p, function ()
                    event:del(conn)
                    conn:close()
        end)
    end)
    end
    local acpt = acceptor:new(9981)
    event:add(acpt, on_new_client)
    event:loop()

几个基本的概念是event/acceptor/connection/connector。上面代码就是创建一个acceptor对象，加到event事件循环中。如果有连接过来，就回会调`on_new_client`函数。

回调函数中的conn参数是一个connection对象，把它加到event中。这个对象的读写操作也是异步的，recv的时候指定回调函数，如果有数据可读，就会被调用。send也类似，不过它是在写操作完成时回调，而不是连接可写时。

event是封装epoll/kqueue做的。没有加timer之类的东西。

------------

lua学起来没太高的门槛。之前大概算零基础吧，一个星期不到，基本是晚上下班和周末零碎的时间。整个学习过程大概是：了解下基本的语法，条件循环分支语句，然后是元表和面向对象的写法的idioms，接着简单看一下跟C的交互部分，主要是栈操作。就可以开始写代码了，这个过程中遇到问题了再去查。

lua跟c的交互实在是太友好了。体验完之后终于明白，在脚本语流行度上，为什么是lua而不是scheme胜出了。scheme也很简单，但是学完语法了什么事情都做不了。为什么？交互不友好。没有各种周边的库，提供的ffi去调c的库也没lua方便。另外编译器的实现太多导致各种分裂，更加剧了问题的严重。

最有价值的几个东西：闭包，协程，自动内存管理。开始时我尝试直接写个c的异步网络库，后面发现没有闭包，没有自动内存管理，在c中写回调代码简直就是灾难。为了说明这一点，看个例子：

    static void
    on_readable(struct packet *p, struct connection *conn, struct event* e) {
        // 为什么会有conn甚至event参数？
        // 假设下面代码里要将conn从event中移除，不传这些额外参数是不行的
        // 其实是闭包中的upvalue
    }

在lua中有闭包的概念，event在函数里面就是一个upvalue，不必要写在回调函数的参数。但是c没有闭包，那么就需要将用到的对象以额外参数过来。这还没完，假设回调是嵌套多层的情况，问题会更严重，闭包参数会不断地增多。

更糟糕的事情是要手动管理内存资源。对于：

    on_readable(struct packet *p, struct connection *conn, struct event* e)

参数的内存到底该怎么分配呢? 如果是确定多层回调中，函数不会返回，那么在栈上分配内存是c常用的优化技巧，甚至用alloca，还不用释放。但这样子是不行的，因为不能保证所有地方都可以这样用。那如果有的地方是分配的，有的地方是栈上的，管理起来就是灾难了。好吧，那我们全部统一用malloc？该什么时候释放是个问题。资源的生命期比较难界定，处理起来是很蛋疼的事情，可以约定调用者分配，回调函数中释放。总之，会有太多的精力在这些事情上面。

----------

练习中遇到的一个问题是如何将lua函数保存下来，后面某个时刻在c中调用。

epoll是要等待有事件可用后，回调的是lua函数。问题是，在c中怎样把lua函数保存起来呢。翻了半天文档，发现栈操作相关函数，有tonumber，tostring之类的，却没有toluafunction。

想了一个野路子解决：用一个全局的元表来做数据交换。把要回调的lua函数放在全局的lua对象中，在c里面记录下index。后面使用时，通过栈交互从全局对象中把函数取出来。刚学也不懂按lua标准idioms该怎么搞，反正it works。

初看觉得不能在c中把lua栈上的函数拿出来觉得有点脑残。思考后才发现，这个设定其实非常精妙。lua跟c的交互都要通过栈，不让c代码引用到lua中的对象。这样子做，在处理垃圾回收是会好很多的。

作为对比，chibi-scheme就有问题。由于允许在c中引用到scheme中的对象，为了防止c引用到的对象被GC干掉，它提供了一套宏机制，来声明这个对象被c引用了，暂时不要回收它。用c去写扩展时，代码中到处是这种宏，做得非常丑陋，以至于我严重怀疑，除了作者自己，有没有其它人知道该怎么使用c去做扩展。

------------

前面说过了闭包和自动内存管理对于写回调函数的重要性。这里再说说协程。协程真是好东西！

实现了上面的异步接口`connection:recv(callback)`。假设我不喜欢异步，想要同步接口怎么办？我就想来一个请求开一个协程去处理请求，当它阻塞时自动切到其它协程去执行。非常简单：

    function connection:read()
        local co = coroutine.running()
        connection:recv(function (p)
            coroutine.resume(co, p)
        end)
        coroutine.yield()
    end

直接将当前的协程给yield掉，执行回调函数的时候在resume回来。这不就是Go的网络接口么：用户层阻塞而底层不阻塞，对用户非常友好，nice!

说到这里，又想跟Go的对比一下。Go的网络只提供了同步接口。用异步实现同步容易，上面的例子中已经说明了。那如果要同步实现异步呢？

假设在要在Go中提供类似`connection:recv(callback)`，那做法是后台开goroutine做读写，然后用channel去做select，select出来做回调。问题是，这样做明显太绕了，每个io连接就多几个goroutine，还有channel的通信开销。所以可以说，在这个场景的网络处理lua是可以做的比Go灵活和轻量的。

为什么云风看过Go并没有转向这门语言。应该就是，吸引力不够。一个精通c的人拿lua可以很容易做一套类似Go的东西出来。正如skynet里有erlang的影子，却用的lua实现，更小更可控。

就写这些了...试玩一把，感觉lua挺不错的。
