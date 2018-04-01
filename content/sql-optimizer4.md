这一次讲谓词下推。谓词下推是相当重要的一个优化。比如 `select * from t1, t2 where t1.a > 3 and t2.b > 5` ，假设 t1 和 t2 都是 100 条数据。如果把 t1 和 t2 两个表做笛卡尔集了再过滤，我们要处理 10000 条数据，而如果先做过滤条件，那么数据量就会少很多。这就是谓词下推的作用，我们尽量把过滤条件下靠近叶子节点做，可以减少访问许多数据。

谓词下推实现的接口函数大概是这样子：

    func (p *baseLogicalPlan) PredicatePushDown(predicates []expression.Expression) ([]expression.Expression, LogicalPlan)

函数会处理当前的算子 p，假设在 p 上层要添加 predicates 这些过滤条件。函数返回的下推之后，还剩下的条件，以及生成的新 plan。这个函数会把能推的尽量往下推，推不下去了剩下的，加一个 Selection 条件在当前节点上面，变在新的 plan。比如说现在有条件 a > 3 AND b = 5 AND c < d，其中 a > 3 和 b = 5 都推下去了，那剩下就接一个 c < d 的 Selection。

DataSource 算子很简单，会直接把过滤条件加入到 CopTask 里面。

我们看一下 Join 算子是如何做谓词下推的。

首先会做一个简化，将左外连接和右外连接转化为内连接。什么情况下外连接可以转内连接？

左向外连接的结果集包括左表的所有行，而不仅仅是连接列所匹配的行。如果左表的某行在右表中没有匹配行，则在结果集右边补 NULL。谓词下推时，外连接转换为内连接的规则：如果我们知道接下来的的谓词条件一定会把包含 NULL 的行全部都过滤掉，那么做外连接就没意义了，可以直接改写成内连接。

什么情况会过滤掉 NUll 呢？比如，某个谓词的表达式用 NULL 计算后会得到 false，比如说谓词里面用 OR 多个条件，其中有条件会过滤 NULL，又或者用 AND 条件连接，并且每个都是过滤 NULL 的。

用 OR 连接起来的条件这个说法比较 low，有个高大上的术语叫析取范式 DNF (disjunctive normal form)。对应的还有合取范式 CNF (conjunctive normal form)。反正如果看到代码里面写 DNFCondition 之类的，知道术语会好理解一些。

接下来，把所有条件全收集起来，然后区分哪些是 Join 的等值条件，哪些是 Join 需要用到的条件，哪些全部来自于左孩子，哪些全部来自于右孩子。

区分开来之后，对于内连接，可以把左条件，和右条件，分别向左右孩子下推。等值条件和其它条件保留在当前的 Join 算子中，剩下的返回，也就是在这个 Join 上面接一个 Selection 节点作为新的 Plan 返回。

谓词下推不能推过 MaxOneRow 和 Limit 节点。

先 Limit N 行，然后再做 Selection 操作，跟先做 Selection 操作，再 Limit N 行得到的结果是不一样的。比如数据是 1 到 100，Limit 10 就是从 1 到 10，再 Select 大于 5 的，一个得到的是 1 到 10，另一个得到的是 5 到 15。MaxOneRow 也是同理，跟 Limit 1 效果一样。

说一下构建 unique key 和 MaxOneRow 属性。这个不属于优化的步骤，但是优化过程需要用来这里的一些信息。

收集关于唯一索引的信息。我们知道某些列，是主键或者唯一索引，这种情况一列不会在多个相同的值。构建 unique key 就是要将这个信息，从叶子节点，传递到 LogicalPlan 树上的所有节点，让每个节点都知道这些属性。

对于 DataSource，对于主键列，和唯一索引列，值都具有唯一属性。注意处理 NULL，考虑列的 NotNull 标记，以及联合索引。

对于 Projection，它的孩子中的唯一索引信息，再到它投影表达式里面用到的那些。比如 a b c 列的值都具备唯一属性，投影其中的 b 列，则输入仍然具有值唯一的属性。

哪些节点会设置 MaxOneRow 信息？

* 如果一个算子的孩子是 MaxOneRow 算子，那它就也带上 MaxOneRow 属性，而且会继续往上传递。
* 如果 Limit 只有一列，它就可以设置 MaxOneRow 属性。
* 如果是 Selection，并且过滤条件是 unique key 等于某个常量，或者 unique key 是某个关联列，那么它也具有 MaxOneRow 属性。
* 对于 Join，如果它的左右孩子都是 MaxOneRow 属性，那 Join 出来仍然会有 MaxOneRow 属性。
