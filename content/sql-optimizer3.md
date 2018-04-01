## 最大最小消除

select min(id) from t

换一种写法，可以变成

select id from t order by id desc limit 1

严格意义上，这并不算标准的逻辑优化里面需要做的事情。那么这个改动有什么意义呢？

前一个语句，生成执行计划，是一个 TableScan 上面接一个 Aggregation，也就是这是一个全表扫描的操作。对于后一个语句，TableScan + Sort + Limit，在某些情况，比如 id 是主键或者是存在索引，数据本身是有序的， Sort 就可以消除，最终变成 TableScan 或者 IndexLookUp 加 Limit，这样子就不需要全表扫了，读到第一条数据就得到结果！全表扫跟只查一条数据，这可是天壤之别。也许这一点点写法上的区别，就是几分钟甚至更长，跟毫秒级响应的差距。

最大最小消除，做的事情就是由 SQL 优化器“自动”地做这个变换。比如说，

select max(id) from t

生成的查询树会被转换成下面这种：

select max(id) from (select id from t order by id desc limit 1 where id is not null) t

min也是类似：

select min(id) from t

select min(id) from (select id from t order by id limit 1 where id is not null) t

注意这只是其中一个变换规则，经过这步变换之后，还会执行其它变换。所以中间生成的算子，还有可能在其它变换里面被继续修改掉。总之，这是一个挺好的例子，说明逻辑优化做的事情：对这颗树，通过一系列规则，做各种变换，得到一个最优的逻辑查询计划。

## 投影消除

投影消除把不必要的 Projection 算子给消除掉。那么，什么情况下，投影算子是可消除的呢？

首先，如果 Projection 算子要投影的列，跟它的孩子结点的输出列，是一模一样的，那么投影就是一个废操作，可以干掉。比如说 select a,b from t 在表 t 里面就正好就是 a b 两列，那就没必要 TableScan 上面做一次 Projection 了。

然后，Projection 下面孩子结点又是一个 Projection 这种，那孩子结点这次投影操作就没意义，可以干掉。像 Projection(A) -> Projection(A,B,C) 只需要保留 Projection(A) 就够了。

类似的，在投影消除步骤里，Aggregation 也算某种意义上的投影操作，因为从这个结点出来的都是列的概念了。因此在 Aggregation -> Projection，这个 Projection 也是可以干掉的。

这个代码应该怎么写呢？ 

    func eliminate(p Plan, canEliminate bool) {
          对 p 的每个孩子，递归地调用 eliminate
          如果 p 是 Project 
              如果 canEliminate 为真 消除 p
              如果 p 的孩子的输出列，跟 p 的输出列相同，消除 p
    }

注意 canEliminate 那个参数，它是代表是否处于一个可被消除的“上下文”里面。比如 Projection(A) -> Projection(A, B, C) 或者 Aggregation -> Projection 递归调用到孩子结点 Projection 时，该 Projection 就处理一个 canEliminate 的上下文。

说白了就是，一个 Projection 结点是否可消除，一方面由它父结点告诉它，它是否是一个冗余的 Projection 操作，另一方面是由它自己和孩子结点的输入列做比较，看是否是可消除的。

[下一篇](sql-optimizer4.md)
