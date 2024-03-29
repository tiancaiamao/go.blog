度量延迟的时候，只用一个 avg 是不够的，需要知道整体的延迟的分布情况。直方图(histogram)用得比较多，更能够客观地反映延迟。
直方图有个不好的地方，就是实现成在线更新比较麻烦。事先不知道整体的分布的时候，预先切分桶也不容易。

今天注意到一个东西叫 [eCDF](https://en.wikipedia.org/wiki/Empirical_distribution_function)，经验累积分布函数(empirical Cumulative Distribution Function)。我发现它具备比直方图更优的一些性质。

- 实现特别简单

这货的表示，其实就是一个排序的数组！

Y轴的范围是 0-1，相当于对应成数组的 length 压缩成 0-1

![image](https://user-images.githubusercontent.com/1420062/188537551-13f4e765-a325-43da-ba28-4c9159f11106.png)

X 轴是值，也就是数组里面存的数值了。

计算 P80, P99 这类操作也特别简单，就是取数组的 (80% 或 99%) * length 对应的位置的值。

![image](https://user-images.githubusercontent.com/1420062/188538343-5bc84f7e-8901-4807-86bd-a9bb6cd6a41e.png)

如果图形很陡，在很小的一段 x 范围内，y 轴快速上升，说明大量的数据是分布在这一块的。比如上面图中的第一条线就是密积分布在 0-25 区间上的，而且在越居中(12.5)的地方，分布得越多，像指数分布。假设如果是均匀分布，它的图形会是一条斜率不变的直线。

- 无损

直方图因为要分 bin，它是损失了原始信息的。bin 怎么分是一个问题，比如 1~2 有一个 bin，原始数据是 1.2 1.3 1.6 1.7 ... 最后都变成了 1~2 之间有 5 个数据，具体 bin 内部的信息就丢失了。

而 eCDF 不存在这样的问题，它存储原始数据，不涉及到分 bin，于是不会损失信息。

- 在线更新

数组嘛，来一个新的记录，直接放进去就完事了。不涉及直方图那种，如果预先划分的 bin 不好，还要动态调整。

如果数据量很大的情况下，可以做成取样的，而取样算法也是可以很方便做成在线更新的。




看看实际的应用，我发现 Go 的 trace 工具里面，GC 相关的 Minimum mutator utilization 其实就是用 eCDF 表示的。
比如以 tidb 跑 sysbench 收集到的信息为例：

```sh
curl http://127.0.0.1:10080/debug/pprof/trace?seconds=3 > trace.out
go tool trace trace.out
```

下面这张图是 Mark assist 对 goroutine 卡顿时间的 eCDF。x 轴是从 3.363913ms，它的意思是说，

- Mark assist 最少都会造成 3.36ms 的卡顿
- 3-8ms 的数据，大概占到了 50%
- 0.943% 的，影响都在 100ms 以内
- 影响最长的时间，要在 1s 级别了，不过比例 99.5% 都会在 1s 以内

![image](https://user-images.githubusercontent.com/1420062/188539718-7e2b0fa6-a46d-48f0-905e-ee7aec3f12bc.png)


然后对比这张图，STW 对 goroutine 的卡顿时间的。

![image](https://user-images.githubusercontent.com/1420062/188540428-e9f3c94a-4480-43dc-8583-784871086650.png)

- STW 最少的一次是 932.6us，也就是它的影响是小于 Mark assist 的
- P95 大概在 23.1ms 
- 99.5% 都是在 247.7ms 以内


另外想到的一个地方是数据库统计信息。统计信息可能还是用直方图比 eCDF 会好一些。因为数据库统计信息的数据量太大了，用 eCDF 做取样可能样本占空间比较大才能准确，这样占的内存不少。而用 histogram 表示应该是更省内存的。
