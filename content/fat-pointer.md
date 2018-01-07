C 太底层了，写代码真的不舒服。不爽的地方之一是要手动管理内存，另一个是对象信息缺失。我们看一个例子，假设要实现一个 hash 表。

	struct HashEntry {
		int key;
		int value;	
	};

如果我们希望是一个更通用的 hash 表。那么 hash 表的 key 和 value 应该是 `void*` 类型。一旦 key 是一个指针以后，我们就必须考虑内存管理。如果我们是一个浅拷贝语义，那么根据谁分配谁释放的原则，这块内存就应该是由外面去释放的。然而这里立马会遇到一个问题：如果 hashEntry 里面把 key 浅拷贝了一份，外面把那块内存释放了，这里就会出现悬挂指针(dangling pointer)。所以这里不能是一个浅拷贝语义。

假设我们是一个深拷贝语义，会面临什么问题？ 首先，对象的递归拷贝性能和实现上都是不靠谱的。需要所有的对象里面引用的对象，都实现一个clone的方法。其次是释放，有clone就要有释放，于是释放函数也要全部有。要释放两次，外面一次，在 hash 析构的时候还要做一次，把 entry 给释放掉，如果忘记了释放，就泄漏了。

所以这里我们最好是一个move语义。一旦把 key value 放到 hash 里面之后，内存管理的归属就转到 hash 里面了。外部如果还持有，那也只是一个引用。我说过 rust 心智负担太重，从这个角度看是有一点不公正的。作用域/生命期/ownship，问题摆在那里，不管有没有制造那么多概念，手动管内存还是得考虑的。

但是问题并没有完，如果 key 是简单的对象，比如 `char*`，我们只需要 free 就好了。而如果是复杂对象，它里面还引用其它的东西，那就需要有一个析构函数。最终这个 hash 会很丑：

	typedef bool (*eq_func) (void*, void*);
	typedef int (*hash_func) (void*);
	typedef void (*free_func) (void*);
	void new_hash(eq_func, hash_func, free_func)

丑的原因是什么呢？根源之一是对象信息缺失，一个通用对象必须是 `void*` ，如何做 key 判等，如何得到一个 hash 值，如何释放对象，这些都必须额外提供。

好，请今天的主角出场：fat pointer !

fat pointer是用来解决对象信息问题的。它的基本原理是这样，分配一个对象出去时，故意在返回地址的前一个地址预留一个空间，也就是多分配一个指针的空间，在那里面放上对象类型信息。对象类型信息有点像 vtable。

	var alloc(size_t data_size) {
	  struct Header* head = calloc(1, sizeof(struct Header) + data_size);
	  head->type = type;
	  return ((var)head) + sizeof(struct Header);
	}

注意，使用 fat pointer 之后释放的时候，就不能直接释放原指针了，要把 free 的位置前移一个。这些东西可以封装起来，提供比较友好的库，[Cello](http://libcello.org/) 已经这么干了。

fat pointer 只是加入了额外的对象信息，但是并不会干扰普通的 C 代码，并不是强制使用，这一点还是很 neat 的。有了对象信息之后，实现通用的 hash 表，就友好多了。因为 `eq_func`，`hash_func`，`free_func` 那一堆东西都可以跟对象绑定到一起，少了很多维护负担。

有了对象信息之后，在这个基础之上实现 GC 是不是一个好主意呢？我认为不是。在 C 里面实现 GC ，获取 `scan_root` 是比较 trick 的。另外，GC 需要对象信息。而如果在 fat pointer 对象里面混入了原生 C 对象，在 scan 遍历对象信息时就会 panic。像无论是 GC，或者是 coroutine，我认为这些东西最好丢到一个虚拟机层去做。但是真这么做也会涉及到另一个问题，托管内存跟原生内存的交互。

托管内存跟原生内存的交互，只在需要的时候使用托管内存，不使用就不为性能买单。理想很美好，现实很骨感。

这里需要一条约束：原生内存不能以任何形式去引用到托管内存。因为托管内存随时可能被 GC 掉，如果原生内存里面引用了，就可能出现悬挂指针了。我只发现了一门语言真正把这个事情做对了，就是 lua。做对是做对了，然而交互的时候 API 真的还是不太爽。shen-go 暂时不敢用 C 重写，担忧的点就包括这些。但是很多底层的东西，像threaded code，fixnum tagging用 Go 都是做不了的。

Anyway，fat pointer 提供对象信息，这个想法是挺不错的。Cello 库使用基本功能，也是挺不错的。
