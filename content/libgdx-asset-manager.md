## 游戏加载到98%卡死

最近开发中遇到的一个问题是，游戏加载到98%时会卡住。原因是我在afterload函数中做太多事了。

游戏的资源管理我单独地写了一个Resource类，其中封装了libgdx的AssetManager，在类的init时调用AssetManager的load函数，然后在loading界面中的update函数中会调用AssetManager的update。最后当AssetManager加载完毕后，调用afterload进行一些后处理工作。

游戏中的UI，音乐，音效，等资源都是用AssetManager加载的，后面在afterload的函数中，只是重新处理了一个映射关系，问题不大。分析一下主要的开销在于我自己写的flash动画类的加载。其加载的代码大概是这样：

	Xfl xfl = xflParser.parse(Gdx.files.internal("animation/block_orange.xml"));

	HashMap<String, TextureRegion> block_orange_map = new HashMap<String, TextureRegion>();
	block_orange_map.put("baozha_0000_1", animation.findRegion("block_orange/LIBRARY/baozha_0000", 1));
	block_orange_map.put("baozha_0007_11", animation.findRegion("block_orange/LIBRARY/baozha_0007", 11));

	FAnimation animation = new FAnimation(xfl, block_orange_map, 100, 100, FAnimation.NORMAL);
	flash_animation.put(Var.ANIMATION_BLOCK_GRAY, animation);

生成FAnimation会依赖于动画的描述Xfl，这个是由分析xml文件各到，还依赖于里面用到的素材文件，是一个Map<String, TextureRegion>。也就是如下依赖关系：

	xml-> Xfl-> FAnimation <-map <-TextureAtlas

这个过程太耗时了，从外存到内存，然后xml的解析，然后解析后的xml生成我的Xfl结构体，然后还要再关联好Texture生成最终的动画表示。最关键问题，我游戏中有好几十个flash动画，都是在这里一口气加载的！这个是在UI线程中做的，所以导致了屏幕卡死，要停很久才会响应。

## libgdx的AssetManager

去扫了一下AssetManager的源代码，看看能不能给我的FAnimation写个Loader了让它异步加载。

AssetManager中，setLoader是给某个类设置loader。比如自己写一个FAnimationLoader，然后调用

	assetmanager.setLoader(FAnimation.class, new FAnimationLoader(new InternalFileHandleResolver()))

相当于给自定义的类加注册一个加载器，这样AssetManager就知道如何加载这个类别了。

在AssetManager的update中，它会从task队列中拿出一个来，调用task.update()。而task的update是这样一个函数：

	public boolean update () {
		ticks++;
		if (loader instanceof SynchronousAssetLoader) {
			handleSyncLoader();
		} else {
			handleAsyncLoader();
		}
		return asset != null;
	}

也就是根据loader的类型做不同的处理。loader就可以是用户自定义的啦。就以handleSyncLoader为例看一下，它是这样子的：

	private void handleSyncLoader () {
		SynchronousAssetLoader syncLoader = (SynchronousAssetLoader)loader;
		if (!dependenciesLoaded) {
			dependenciesLoaded = true;
			dependencies = syncLoader.getDependencies(assetDesc.fileName, assetDesc.params);
			if (dependencies == null) {
				asset = syncLoader.load(manager, assetDesc.fileName, assetDesc.params);
				return;
			}
			for (AssetDescriptor desc : dependencies) {
				manager.injectDependency(assetDesc.fileName, desc);
			}
		} else {
			asset = syncLoader.load(manager, assetDesc.fileName, assetDesc.params);
		}
	}

如果这个loader有dependency，则把它的dependency加到asset里面，但是把dependenciesLoaded设为true了。我自己尝试写了一个FAnimationLoader，跟代码到这里，发现有bug。因为它并没有保证一个类别的加载是在它的依赖加载完成之后才进行的。

比如说，我在FAnimationLoader的getDependencies函数中加入map，然后在load函数中生成Xfl并new一个FAnimation。这时却发现map并没准备好。原因就像上面说的，libgdx的AssetManager框架的加载，并没有保证一个东西的dependency加载完之后才去加载它。就像代码中所示，它的做法就是直接去加载那个东西，如果发现它有dependency，那么把dependency丢到asset中就算已经加载了。

折腾了差不多一整天，都没搞定这个问题。搞不定，按AssetManager的规则写Loader弄出来没能正常工作，自己去重写一个AssetManager的代价又有点高。它的整个结构就是一个任务队列，加个异步的线程池，相当于一个小的调度系统了。最蛋疼的是加载过程中有依赖处理，不是从任务队列中取一个就拿去执行了，这个任务可能是带依赖的，需要先处理好依赖。

## 穷人的解决方案

给FAnimation写个完善的loader太疼了，自己去另做一个AssetManager更疼。想了一下穷人的解决方案，明天去试一下。

思考一下FAnimation中的耗时的部分，虽然细节我没去统计，但从程序员的直觉上能感觉出来，主要部分无非是Gdx.files.internal的io开销，以及我的FAnimation构造函数中的cpu开销。

我可以只写一个Xfl的loader，这个几乎相当于一个xml的loader，应该简单很多。但是却很有用，有这个loader之后就可以把afterloader中的io开销给省掉了。然后我可以把afterload函数单独开一条线程做，这样就解决了cpu方面，占住UI线程的问题。这么弄之后有个问题就是玩家进入界面后，也许后台线程还在做afterloading，资源并没真的准备好。不过没事，可以给Resource的get函数封装一下，如果get为某项为空，就让它对这一项建立一下...

今天是七夕，别人都出去玩了，哥居然在这里攻克技术问题，屌丝命苦啊，F*CK!
