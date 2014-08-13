用tcpcopy导的线上流量，活动数据库也是真实的数据，这是我抓的一份pprof：

	genius@geniuss-Macbook-air:~/project/src/dsp_masky $go tool pprof ls.prof 
	Welcome to pprof!  For help, type 'help'.
	(pprof) top
	Total: 14413 samples
	    1342   9.3%   9.3%     1342   9.3% strconv.ParseUint
	    1243   8.6%  17.9%     1243   8.6% runtime.duffcopy
	    1082   7.5%  25.4%     5067  35.2% dsp_masky/model/bid.(*BidServer).FilterByCampaign
	    1073   7.4%  32.9%     1824  12.7% runtime.mallocgc
	     638   4.4%  37.3%      638   4.4% runtime.MSpan_Sweep
	     555   3.9%  41.2%      555   3.9% settype
	     547   3.8%  45.0%      547   3.8% runtime.mapaccess2_fast64
	     513   3.6%  48.5%     1855  12.9% strconv.ParseInt
	     376   2.6%  51.1%      452   3.1% regexp.(*machine).add
	     337   2.3%  53.5%     2418  16.8% dsp_masky/model.regionQuery

消耗排前三的函数分别是ParseUint，duffcopy，FilterByCampaign。下面一条一条的分析。

# 优化duffcopy

我查了一下duffcopy这个函数，是一个类似memcopy的函数，将内存从一块复制到另一块。这个函数并没有由runtime中的任何库函数调用到，它是编译器生成的代码直接调用的。
为什么duffcopy如此之高？我在看mentor的编程习惯的时候找到了原因：

	var RCampaign map[int64]Campaign

RCampaign是一个全内存的活动数据库，注意到它使用的存储是`Campaign`而不是`*Campaign`，其中`Campaign`的定义是这样子的，很大的一个结构体：

	type Campaign struct {
	    Id                int64
	    Plan              CPlan
	    PublisherId       int
	    CreateUserId      int64
	    WardenStatus      int
	    Zone              []Zone
	    IpSections        []IpSection
	    ChannelId         int64
	    AdxId             int64
	    CategoryId        []int
	    TopPrice          float64
	    SubPrice          SubCharge
	    PositionPrice     map[int64]ChargePrice
	    ChargePrice       float64
	    ChargeType        int
	    Gaga              FilterGaga
	    MediaFilter       MediaFilter
	    OType             int
	    FrequencyFilters  []model.FrequencyFilter
	    AMonit            []string
	    Character         []model.ActionTerm
		...
	}

mentor平素是这样子写代码的：

	func ConvertCampaign(campaignInfo model.Campaign) (campaign Campaign, err error) {	//直接返回这么大的结构体
	campaigns[Id] = campaign	//直接整个结构体赋值到map中

类似的例子还有很多，这些都会造成整份的数据拷贝，正是这样的编程习惯导致了duffcopy函数消耗了8.6%的CPU！

推荐的一个好的习惯是，稍大的类型存到map都存储指针而不是值。

下面是优掉掉duffcopy之后抓的一份pprof：

	Total: 12507 samples
	    1739  13.9%  13.9%     1739  13.9% strconv.ParseUint
	    1290  10.3%  24.2%     8282  66.2% dsp_masky/model/bid.(*BidServer).FilterByCampaign
	     813   6.5%  30.7%      906   7.2% runtime.mapaccess2_fast64
	     705   5.6%  36.4%     2444  19.5% strconv.ParseInt
	     440   3.5%  39.9%      523   4.2% regexp.(*machine).add
	     402   3.2%  43.1%     1202   9.6% hash_insert
	     394   3.2%  46.2%     3136  25.1% dsp_masky/model.regionQuery
	     305   2.4%  48.7%     2737  21.9% strconv.Atoi
	     305   2.4%  51.1%      316   2.5% syscall.Syscall
	     288   2.3%  53.4%     1027   8.2% runtime.mallocgc

# 优化ParseUint

这是我们实时竞价服务，每秒会有大量的请求到达。在每一次请求中，需要遍历所有活动，挑选出跟这次曝光机会相匹配的，参与竞价。其中有一项是地域信息，请求中会包含IP来源。而活动中是有地域信息，通过IP查询到用户所属地域，然后跟活动中地域进行比较。

问题出在地域使用的结构体上，活动中存储是用string表示的：

	type Zone struct {
	    CountryId string
	    RegionId  string
	    CityId    string
	}

而由IP得到的地域是用int表示的。所以比较两者是否匹配时，会用`strconv.Atoi`进行转换，这个函数调用了ParseUint。

当前的活动数量是2000，其中实际参与比较的是400个，然后每次比较会调用三次字符串转换操作，假设QPS当前是1500，那么400\*3\*1500将产生1,800,000次的ParseUint调用。这就是ParseUint函数会占用这么高CPU的原因了。

所以，我将Zone的存储结构调整为int后，避免了ParseUint函数。优化掉ParseUint之后，得到的pprof是这样子的：

	(pprof) top
	Total: 4554 samples
	     426   9.4%   9.4%      480  10.5% runtime.mapaccess2_fast64
	     232   5.1%  14.4%     1768  38.8% dsp_masky/model/bid.(*BidServer).FilterByCampaign
	     216   4.7%  19.2%      216   4.7% syscall.Syscall
	     183   4.0%  23.2%      213   4.7% hash_insert
	     167   3.7%  26.9%      167   3.7% ExternalCode
	     141   3.1%  30.0%      222   4.9% runtime.mallocgc
	     121   2.7%  32.6%      121   2.7% runtime.futex
	     115   2.5%  35.2%      136   3.0% evacuate
	     112   2.5%  37.6%      112   2.5% runtime.MSpan_Sweep
	     103   2.3%  39.9%      103   2.3% runtime.aeshash64

刚拿到代码的时候观察，极限的时候大概会有2000多一点的QPS，在最高的流量压力下CPU占用甚至会超过700%，而做完这两个做化之后，再也没看到过CPU跑到500%以上。

# 干掉defer

`mapaccess2_fast64`是由于`v, ok := m[key]`这种形式的map访问产生的。当前代码中非常大量地使用了map数据结构，而我看了一下相应的代码，大多使用都属于合理的范畴，所以这个方向很难优化下去。也许换成自己实现的hash会带来一定的提升，但是吃力不讨好的事情，暂时放着。

至于FilterByCampaign，对于每个曝光机会，需要与所有的活动的设定进行匹配，进行过滤，里面有一个大的`for range`循环很耗性能。除非算法级别的优化，否则这个大循环都避不开。但我暂时还没想到一个好的算法。

有点进展不顺了，于是去与mentor讨论。mentor提到，在加入了调价的一个更新之后，系统的CPU从原来的200%升到了400%的样子，让我可以去查一下这条路径的代码。然后我看到了defer，立马眼睛亮了：

	 func (this *Campaign) GetPrice(positionId, size, host int64, b *BidServer) (price float64, err error) {
	     ...
	     defer func() {
	         ...
		 }()
	
		 ...
	     if err != nil {
	         return
	     }
	
	     return
	 }

这里有个明显的使用不当：如果返回err不为空，直接返回就可了，完全不需要执行defer函数，所以代码不应该这么写。而且，defer调用会有一定的开销。简要地说一下，首先defer需要把参数之类的东西先封装进一下结构体，跟函数一起，成为一个闭包，保存到defer链中。然后在函数返回时还会有一个defer链表的处理，以栈的顺序调用执行闭包函数。对于defer的垃圾回收等，也会有特殊的一些处理，总之，影响挺大。

GetPrice函数是代码路径中一个必然调用的函数，虽然没有循环，但是QPS上去之后，性能影响不可小嘘。优化完这里后观察，在大概1000左右QPS环境下，CPU使用率从优化前的200%左右，下降到了120%左右。

defer是性能杀手，我的原则是能不用尽量避开。

# time.Date

现在看pprof是这样的：

	(pprof) top
	Total: 3348 samples
	     254   7.6%   7.6%      266   7.9% syscall.Syscall
	     177   5.3%  12.9%      221   6.6% hash_next
	     168   5.0%  17.9%     1320  39.4% dsp_masky/model/bid.(*BidServer).FilterByCampaign
	     153   4.6%  22.5%      369  11.0% runtime.mallocgc
	     153   4.6%  27.0%      194   5.8% runtime.mapaccess2_fast64
	     113   3.4%  30.4%      113   3.4% time.absDate
	     102   3.0%  33.5%      102   3.0% runtime.futex
	      90   2.7%  36.1%      106   3.2% runtime.MSpan_Sweep
	      83   2.5%  38.6%       83   2.5% runtime.aeshash64
	      74   2.2%  40.8%       86   2.6% syscall.Syscall6

呐呢？`time.absDate`这个函数能占用到3.4%的性能？赶紧看代码：

     for _, campaignId := range campaignIds {
         //获取活动
         campaign, ok := RCampaign[campaignId]
		 
         //排期
        now := time.Now()
        year, month, day := now.Date()
        day = year*10000 + int(month)*100 + day

改掉，移到for循环外面去，又找了一个问题，好happy。优化完之后的pprof：

	(pprof) top
	Total: 3310 samples
	     272   8.2%   8.2%      278   8.4% syscall.Syscall
	     213   6.4%  14.7%      264   8.0% hash_next
	     172   5.2%  19.8%      385  11.6% runtime.mallocgc
	     163   4.9%  24.8%      215   6.5% runtime.mapaccess2_fast64
	     155   4.7%  29.5%     1045  31.6% dsp_masky/model/bid.(*BidServer).FilterByCampaign
	     143   4.3%  33.8%      143   4.3% runtime.futex
	      90   2.7%  36.5%       90   2.7% runtime.aeshash64
	      87   2.6%  39.1%      116   3.5% runtime.MSpan_Sweep
	      67   2.0%  41.1%       82   2.5% settype
	      66   2.0%  43.1%       66   2.0% runtime.findfunc

这个做完之后，1000QPS左右，CPU的使用率大概到了100%以内，相对上一次大概有个20%的情况优化。

# 优化mallocgc

mallocgc占了5.2%，这个是由于频繁内存分配导致的。跟着svg找到了mallocgc的来源，是由这个函数产生的：

	func (this *Campaign) GetPrice(positionId, size, host int64, b *BidServer) (price float64, err error) {
	        offer = &Offer{
	            CampaignId: this.Id,
	            UserId:     this.CreateUserId,
	            Ctype:      this.ChargeType,
	        }
	        malloced = true
	    }
		...
	}

每秒会来许多的请求，每来一条请求，都会对每个活动调用GetPrice进行价格方面的过滤，正是offer这个出价对象导致了大量的mallocgc。

既然找到了问题，就开始动手改。想法是，专门做一个Offer对象的分配池。

Go1.3提供了sync.Pool，我先用这个去实现，做了一个全局的`var OfferPool *sync.Pool`，然后每次分配从pool中去取。但...这是一次不成功的尝试。修改之后，我发现CPU利用率暴涨近一倍，pprof显示，大量的CPU时间耗在了Pool.Get函数中。

怎么可能做了对象的分配池，性能反而不如每次直接分配了呢？？？

思考之后，找到了原因。大量的CPU耗在Pool.Get中，里面是调用的CAP操作，抢‘锁’的状况很严重。原来，我做的是一个全局的pool，而每个请求都是在不同goroutine中，所以从pool中取对象时，抢锁浪费了大量的CPU，导致优化以后CPU反而暴增了。那么为什么mallocgc却没事呢？是因为它的锁的粒度小得多。mallocgc分配时，goroutine会先从本地的MCache中取内存，取不到时会到结体体M中取内存块，只有当M中出取不到时，才会去全局的内存分配池中取，所以不存在严重的竞争问题。

为了避开全局锁的问题，我改成了每个请求中附带一点Offer缓存，然后分配改成了下面的样子：

    var offer *Offer
    if b.OfferPoolIndex < len(b.OfferPool) {
        offer = &b.OfferPool[b.OfferPoolIndex]
        b.OfferPoolIndex++
    } else {
        offer = &Offer{
            CampaignId: this.Id,
            UserId:     this.CreateUserId,
            Ctype:      this.ChargeType,
        }
    }

由于之前已经优化过许多了，这个优化带来的提升不是很大了，大概在10%吧。目前观察到的1000QPS左右的CPU使用率大概在70-80%。再做下去已经性价比不高了，于是这次的优化工作到此结束！

-----------------

更新：

后面测试发现，在优化ParseUint的时候修改代码引入了错误，导致过滤了很多的计算量。之后的继续优化中，相关数据失效，pprof不再能体现正确的方向。忧郁了~_~

# 优化前后对比

实验环境是开两个进程，一个跑优化前的代码，一个跑优化后的代码。用tcpcopy同时给两个进程导入线上真实流量观察。

## CPU利用率

观察一段时间的两者CPU利用率，这是取样的一小段数据：

优化前：473 473 450 476 469 579 495

优化后：258 264 246 261 258 267 274

平均为原来的53.5%。

## 内存

使用比原来略有降低。

	优化前：{"mem":"508.8MB","ver":"20140327~git-xxx","up_time":"2014-08-04 18:02:55","total_count":50208025,"last_10_second_qps":1119.800000}
	优化后：{"mem":"431.6MB","ver":"20140327~git-xxx","up_time":"2014-08-05 12:12:43","total_count":9974904,"last_10_second_qps":1056.700000}

## 响应时间

目前响应时间落在10ms以内是比较理想的。作为对比观察，tail取100000条日志记录，比较响应时间落在10/20/30ms区间上的记录条数

优化前：7309/1924/779

优化后：4220/1041/220


