Go语言中没有继承，但是可以用结构体嵌入实现继承，还有接口这个东西。现在问题来了：什么场景下应该用继承，什么场景下应该用接口。

## 问题描述

这里从一个实际的案例出发。网游服务器中的一个例子。假设每个实体都有一个ObjectID，packet中都有使用到这个ObjectID，客户端与服务端之间通过这个ObjectID知道是一个什么实体。用面向对象的观点，就是有一个Object对象，里面有`getObjectID()`方法，所有对象都是继承自Object对象。

Creature继承Object，表示游戏中的生物。然后像Monster，NPC，都继承自Creature的。玩家分为三个种族，Slayer/Vampire/Ouster三个不同的类实现，继承自Creature。

Item也继承自Object，表示物品类。除了像装备这种很直观的物品，尸体这类Corpse也是继承自Item的。而尸体又有分MonsterCorpse和SlayerCorpse/VampireCorpse这种玩家尸体。

Effect也继承自Object，表示效果类。比如玩家身上的状态。还有其它很多很多，全是以Object为基类的。

总之，Object是一个最下面的基类，直接的派生类很多，派生类的派生类更多，这样一颗继承树结构。

## 第一次尝试

首先是用继承的方式来。

	type Object uint64
	type Creature sturct {
		Object // Creature继承自Object
	}
	type Monster struct {
		Creature // Monster继承自Monster
	}

这样做的好处是，Monster直接可以调用到Creature里的方法，Creature直接可以调用Object里的方法。不用重写代码，就是继承的好处啦。

但是...Go中没有基类指针指向派生类对象，不可以`*Object`指向一个`*Monster`对象，调用Monster中的方法。

而我实际上在很多地方需要这种抽象类型机制，比如存储需要存Creature类型，使用的时候再具体用Monster类型方法。

## 第二次尝试

这次用接口的方式：

	type Object interface {
		ObjectID() uint
	}

游戏中的每个实体的特征是有一个ObjectID，所以这次把所以实现了ObjectID方法的，都是一个Object。

好啦，这样就可以存不同的Object了：

	objs []Object
	switch objs[i].(type) {
		case Monster:
		case Item:
	}

还是使用继承以实现方法重用：

	type Monster struct {
		Creature // 继承Creature。实现Object
	}

但是...新的问题出现了。比如说给将Monster赋值给Object：

	var obj Object
	var monster Monster
	obj = monster
	
	if _, ok := obj.(Creature); ok {
		// 居然不ok
	}

obj是一个Monster，Monster继承Creature，但是obj却不是一个Creature，what the fuck?

别小看这个问题，因为它满足不了业务逻辑。比如说：

	func f(obj Object) {
		// 如果obj是Creature，做逻辑A
		// 如果obj是Item，做逻辑B
	}
	// obj实际上是Monster，或者Player，或者MonsterCorpse，或Corpse，或WearItem等最具体的派生类
	// 但是没有办法知道obj到底是Creature或者是Item

## 第三次尝试

第二次尝试中，对Object使用了接口，但是不彻底。导致了一个Object是Monster却不是Creature。如果全部接口化会怎么样？

	type Object interface
	type Creature interface {
		Object	// 嵌入Object接口，一个东西实现Creature必须是实现Object的
		XXX()	// Creature自身的方法XXX
	}
	type Monster interface {
		Creature // 一个东西实现Monster必须是实现Creature的
		YYY()	// 成为Monster需要实现的方法
	}

额，好像可以了，现在一个东西实现Object，如果它是Monster，那么一定是Creature。

但是...没有用继承了。对所有最具体的派生类，都需要把所有这写方法实现一遍，这肯定不靠谱。

## 答案

回头想一下，总结问题所在：

* 第一次尝试中，只用了继承，无法满足需求。我需要存储的时候存放基类，使用的时候使用派生类。
* 在第三次尝试中，只使用了接口，可以实现，但是要写很多重复代码。
* 第二次接口与继承都有用到，但是好像不对。

总结下来就是，为了不重写代码，必须使用继承。而为了存储抽象类型，调用具体类型的方法，必须用接口。

好好整理一番好，我找到了正确的方式，把接口和继承分开：

	type ObjectInterface interface {
		// 所有能够返回对象类型的，都实现了Object接口。
		// 包括OBJECT_CLASS_ITEM/OBJECT_CLASS_CREATURE/OBJECT_CLASS_EFFECT等
		ObjectClass() ObjectClass 
	}
	type Object uint64
	
	type Creature struct {
		Object	// 继承Object对象
	}
	func (c Creature) ObjectClass() { // 实现ObjectInterface接口
		return OBJECT_CLASS_CREATURE
	}
	
	type Item struct {
		Object // 继承Object对象
	}
	func (i Item) ObjectClass() { // 实现ObjectInterface接口
		return OBJECT_CLASS_ITEM
	}

CreatureInterface接口也类似。Monster和NPC以及玩家都是实现CreatureInterface接口，并继承Creature对象的。想写的话，让CreatureInterface接口必须是Object。
