[TiDB 的性能挑战赛](https://pingcap.com/community-cn/high-performance-tidb-challenge/)活动。简单记录一下搭建环境和跑 TPCC 测试的过程。

首先是[使用 TiUP 部署集群](https://docs.pingcap.com/zh/tidb/stable/production-deployment-using-tiup)，使用最小拓扑架构，只用了 1 个 TiDB，然后是 3 个 PD 和 3 个 TiKV，Grafana 肯定是要的，分配了 6 台云主机，最终分配如下：

```
10.28.11.181 tiup sysbench pd
10.28.11.182 pd monitor grafana alert
10.28.11.183 pd tidb
10.28.11.184 tikv
10.28.11.185 tikv
10.28.11.186 tikv
```

这里一个小细节是，注意内网外网，活动主办方给的配置信息是外网的，但是在集群内部署都要用内网的。

写好 topology.yaml 之后，执行：

```
tiup cluster deploy tidb-test nightly ./topology.yaml --user root -p
```

然后集群就搭起来了，查看状态正常：

```
tiup cluster display tidb-test
```

感受是，TiUP 真的挺好用，记得以前要搭一个集群老费劲了。讲个笑话是，让新入职的员工把各组件编译好，然后弄好配置，命令行启动，到最终能让一个集群运行起来，一个星期就过去了。不过现在有 TiUP 之后这个看起来好了很多。

搭好集群后，下一件事情是安装 TPCC。TiDB 的官网上有写 [如何对 TiDB 进行 TPC-C 测试](https://docs.pingcap.com/zh/tidb/dev/benchmark-tidb-using-tpcc)，照着上面的教程，clone benchmarksql，然后构建

```
cd benchmarksql
ant
```

一直走到修改配置那里，这个云主机配置比较低，4 核 8G 的，使用官网那个 1000 个 warehouse，500 个终端，32 并发导数据，会 OOM。改成了 64 warehouse，32 终端，并发度 4。导数据有点慢，完成之后，就可以运行测试了：

```
nohup ./runBenchmark.sh props.mysql &> test.log &
```

搭完测试，运行 TPCC，下一步是观察一下 Dashboard，Dashboard 跟 PD 是集成在一起的，通过这个命令的返回，可以看到 Dashboard 所在的 PD：

```
tiup cluster display tidb-test
...
10.28.11.183:2379   pd            10.28.11.183  2379/2380    linux/x86_64  Up|L|UI  /tidb-data/pd-2379            /tidb-deploy/pd-2379
```

然后就是打开浏览器访问 http://10.28.11.183:2379/dashboard (注意一下内网外网 IP)

Dashboard 也是挺好用的，分析 SQL 语句信息，选择 tpcc 数据库，过滤取最近的 30min，可以看到

![](static/dashboard.jpg)

点开还可以看到具体的执行信息。感觉对于做 TPCC 的 workload 的那个组真是太方便了。
