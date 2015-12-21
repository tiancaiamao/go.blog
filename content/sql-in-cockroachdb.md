
本文翻译自cockroach的官方博客。原文地址：<http://www.cockroachlabs.com/blog/sql-in-cockroachdb-mapping-table-data-to-key-value-storage/>

-----

SQL？我以为CockroachDB是一个key-value存储？！？

过去我们将CockroachDB描述成一个分布式的，支持事务一致性的键值存储。我们知道，key-value的API并不是我们最终想提供的。几个月前，我们开始实现更高层次的结构化数据的API，支持表和索引。不仅仅是支持这样更丰富的结构，我们最终期望是能够支持SQL，用于操作和访问结构化的数据。

在一个SQL系统中的许许多多的组件，举几个例子：查询的语法解析，查询分析，查询计划，查询的执行，事务，持久化存储。CockroachDB的SQL系统是构建在CockroachDB内部的key-value存储之上的，并利用单调有序的key-value映射来存储所有的SQL数据和索引。这篇文章将关注CockroachDB中的SQL数据到key-value存储的映射，并展示这个映射是如何帮助实现SQL功能的。后面的文章会谈到查询分析，查询计划和执行。

一张SQL表是一系列的行的集合，每一行又是一些列的集合。 每列都有关联的类型（布尔，整型，浮点，字符串，字节）。 表还关联了索引，索引允许高效地检索表中的一定范围的行。 听起来这一点也不像一个键值API，key-value的API是将字符串的key映射到字符串value。 我们如何将SQL表数据映射成KV存储？

首先，基本的：CockroachDB内部的键值API支持许多操作，不过这篇文章中我们只需要知道其中的几个：

* ConditionalPut(key, value, expected-value) --如果key现有的值与预期值匹配，则将key的值设置为value
* Scan(start-key, end-key) --检索在start-key（包含）和end-key（不包含）之间的所有的key。

在CockroachDB，键和值是可以包含无限制的字节值字符串。 好，让我们继续往下看！

## key编码

将SQL表数据映射成key和value这个问题中，最基本的一块是将有类型的列数据转换为字符串。例如，给定一组值 <1, 2.3, "four”> ，我们想它编码为一个字符串，在概念上看是这样的：

    /1/2.3/"four”

我们使用斜杠作为值之间的分隔符，虽然这仅仅是为了提高可读性。 我们可以用一整篇博客来讲编码是如何工作的; 为了简洁起见，这篇文章只讨论它们的特征，而不是实现。 编码后的key用其它的各个字段进行排序之后：

    /1/2.3/"four” 
    /2/3.1/"six” 
    /10/4.6/"seven”

如果你对这些字符串只是直接排序，你会发现 /10/… 跑到了 /2/…前面。如果你还没有遇到过，到底编码如何工作的会显得有点不可思议。如果你对具体细节很有兴趣，要以看一下 [util/encoding](https://github.com/cockroachdb/cockroach/tree/master/util/encoding) 中的{Encode,Decode}{Varint,Float,Bytes,Null}。

处理完这个编码之后，我们已经准备好了对SQL表数据的编码一窥究竟。 在CockroachDB，每个表都在创建时给它分配了一个唯一的64位整数ID。 这个表ID被用作所有与该表相关的key的前缀。 现在让我们考虑下面的表和数据：

    CREATE TABLE test ( 
        key INT PRIMARY KEY, 
        floatVal FLOAT, 
        stringVal STRING 
    ) 
      
    INSERT INTO test VALUES (10, 4.5, "hello”)

在CockroachDB中，每个表都必须有一个主键。主键由一个或多个列构成; 在上面的test表中，它是一个单列的。CockroachDB用一项key-value来存储每一个非主键的列，用主键和列名作为前缀。在我们的test表中 <10, 4.5, "hello”> 这样一行将被存储为：

<table>
<thead>
<tr>
<td style="width: 70%;"><b>Key</b></td>
<td><b>Value</b></td>
</tr>
</thead>
<tbody>
<tr>
<td><span style="font-weight: 400;"><code>/test/10/floatVal</code></span></td>
<td><span style="font-weight: 400;"><code>4.5</code></span></td>
</tr>
<tr>
<td><span style="font-weight: 400;"><code>/test/10/stringVal</code></span></td>
<td><span style="font-weight: 400;"><code>"hello”</code></span></td>
</tr>
</tbody>
</table>

在这个描述中，我们使用/test/作为一个表ID的占位符，/floatVal和/stringVal作为列ID的占位符（表中的每一列都有一个ID，列ID在表内是唯一的）。 注意，主键紧跟在我们的编码的表ID。这是CockroachDB的SQL实现中索引扫描的基础。

如果我们想知道事物的本质，我们将看到表的元数据：

<table>
<thead></thead>
<tbody>
<tr>
<td style="width: 70%;"><span style="font-weight: 400;"><code>test Table ID</code></span></td>
<td><span style="font-weight: 400;"><code>1000</code></span></td>
</tr>
<tr>
<td><span style="font-weight: 400;"><code>key Column ID</code></span></td>
<td><span style="font-weight: 400;"><code>1</code></span></td>
</tr>
<tr>
<td><span style="font-weight: 400;"><code>floatVal Column ID</code></span></td>
<td><span style="font-weight: 400;"><code>2</code></span></td>
</tr>
<tr>
<td><span style="font-weight: 400;"><code>stringVal Column ID</code></span></td>
<td><span style="font-weight: 400;"><code>3</code></span></td>
</tr>
</tbody>
</table>

换成数字的形式，我们的表的键值对如下所示：

<table>
<thead>
<tr>
<td style="width: 70%;"><b>Key</b></td>
<td><b>Value</b></td>
</tr>
</thead>
<tbody>
<tr>
<td><span style="font-weight: 400;"><code>/1000/10/2</code></span></td>
<td><span style="font-weight: 400;"><code>4.5</code></span></td>
</tr>
<tr>
<td><span style="font-weight: 400;"><code>/1000/10/3</code></span></td>
<td><span style="font-weight: 400;"><code>"hello”</code></span></td>
</tr>
</tbody>
</table>

这篇文件的后面部分，我们还是使用符号的形式。

[你可能会觉得每个key都存储共同的前缀(/1000/10)浪费存储，但我们的底层存储引擎RocksDB通过key的前缀压缩，消除了几乎所有的开销。]

细心的读者会注意到，对于那些包含在主键中的列，存储key-value是没有必要的，因为这些列的值已编码在key本身了。事实上，CockroachDB确实删掉了这些。

注意到对于特定的某一行，由于使用的主键前缀，它的所有的key在存储中都是相邻的（记住，在CockroachDB中key-value数据是存储为一个单调有序的map，所以这项特征是“免费”获得的）。通过使用前缀扫描操作，就可以检索特定的一行的所有值了。CockroachDB内部正是这样实现的。

查询：

    SELECT * FROM test WHERE key = 10

将被翻译成：

    Scan(/test/10/, /test/10/Ω

这个扫描将检索到这行的两个key。 然后查询执行引擎会将key解码来重构获得这一行。

## 空列值

故事到这里还有一点点小问题：除非明确地标记为NOT NULL，列的值可以是NULL。CockroachDB不会存储NULL值，而是利用key-value不存在来表示一个空列。细心的读者会注意到这里一个细节：如果某一行中所有的非主键列全是NULL，这一行我们不会存储任何键（译者注：无法知道存在这样一行）。为了解决这种情况，CockroachDB总是为每一行写入一个不带列ID后缀的主键作为哨兵。在我们的例子中 <3, 4.5, "hello”> 这样一行的的哨兵key是/test/3。哇！

## 二级索引

到目前为止，我们忽略了二级索引。 让我们来纠正这个疏忽：

    CREATE INDEX foo ON test (stringVal)

这将创建为stringVal列创建二级索引。我们还没有声称该索引是唯一的，因此重复值是允许的。类似的表中的行，我们以表ID为前缀来存储所有的索引数据。但我们想要区分索引数据和行数据。我们通过引入一个索引ID来实现，索引ID在每个表中是唯一的，包括主键索引（抱歉，前面我们撒谎了！）：

    /tableID/indexID/indexColumns[/columnID]

上面我们作为例子的key变得稍微长了一些：

<table>
<thead>
<tr>
<td style="width: 70%;"><b>Key</b></td>
<td><b>Value</b></td>
</tr>
</thead>
<tbody>
<tr>
<td><span style="font-weight: 400;"><code>/test/primary/10</code></span></td>
<td><span style="font-weight: 400;"><code>Ø</code></span></td>
</tr>
<tr>
<td><span style="font-weight: 400;"><code>/test/primary/10/floatVal</code></span></td>
<td><span style="font-weight: 400;"><code>4.5</code></span></td>
</tr>
<tr>
<td><span style="font-weight: 400;"><code>/test/primary/10/stringVal</code></span></td>
<td><span style="font-weight: 400;"><code>"hello”</code></span></td>
</tr>
</tbody>
</table>

现在，对于我们的foo索引也有一个key：

<table>
<thead>
<tr>
<td style="width: 70%;"><b>Key</b></td>
<td><b>Value</b></td>
</tr>
</thead>
<tbody>
<tr>
<td><span style="font-weight: 400;"><code>/test/foo/”hello”/10</code></span></td>
<td><span style="font-weight: 400;"><code>Ø</code></span></td>
</tr>
</tbody>
</table>

你可能会觉得奇怪，为什么我们在编码后缀中带上了主键值(/10)。对于非唯一索引foo这是必要的，以允许多个行出现相同的值。 由于主键在表中是唯一的，在它后面追加一个后缀非唯一的后缀会得到一个唯一的key。 通常，对于一个非唯一索引，CockroachDB会将所有 包含在主键中却不包含在索引中的 列的值追回进去。

现在让我们看看，如果我们在表中插入 <4, NULL, "hello”> 会发生什么：

<table>
<thead>
<tr>
<td style="width: 70%;"><b>Key</b></td>
<td><b>Value</b></td>
</tr>
</thead>
<tbody>
<tr>
<td><span style="font-weight: 400;"><code>/test/primary/4</code></span></td>
<td><span style="font-weight: 400;"><code>Ø</code></span></td>
</tr>
<tr>
<td><span style="font-weight: 400;"><code>/test/primary/4/stringVal</code></span></td>
<td><span style="font-weight: 400;"><code>"hello”</code></span></td>
</tr>
<tr>
<td><span style="font-weight: 400;"><code>/test/foo/"hello”/4</code></span></td>
<td><span style="font-weight: 400;"><code>Ø</code></span></td>
</tr>
</tbody>
</table>

所有的表数据放一起看：

<table>
<thead>
<tr>
<td style="width: 70%;"><b>Key</b></td>
<td><b>Value</b></td>
</tr>
</thead>
<tbody>
<tr>
<td><span style="font-weight: 400;"><code>/test/primary/4</code></span></td>
<td><span style="font-weight: 400;"><code>Ø</code></span></td>
</tr>
<tr>
<td><span style="font-weight: 400;"><code>/test/primary/4/stringVal</code></span></td>
<td><span style="font-weight: 400;"><code>"hello”</code></span></td>
</tr>
<tr>
<td><span style="font-weight: 400;"><code>/test/primary/10</code></span></td>
<td><span style="font-weight: 400;"><code>Ø</code></span></td>
</tr>
<tr>
<td><span style="font-weight: 400;"><code>/test/primary/10/floatVal</code></span></td>
<td><span style="font-weight: 400;"><code>4.5</code></span></td>
</tr>
<tr>
<td><span style="font-weight: 400;"><code>/test/primary/10/stringVal</code></span></td>
<td><span style="font-weight: 400;"><code>"hello”</code></span></td>
</tr>
<tr>
<td><span style="font-weight: 400;"><code>/test/foo/"hello”/4</code></span></td>
<td><span style="font-weight: 400;"><code>Ø</code></span></td>
</tr>
<tr>
<td><span style="font-weight: 400;"><code>/test/foo/"hello”/10</code></span></td>
<td><span style="font-weight: 400;"><code>Ø</code></span></td>
</tr>
</tbody>
</table>

在处理SELECT扫描的过程中，会使用二级索引缩小扫描的key的范围。考虑：

    SELECT key FROM test WHERE stringVal = "hello”

查询规划会注意到有上stringVal索引并译成：

    Scan(/test/foo/”hello”/, /test/foo/”hello"/Ω

这将检索到keys：

<table>
<thead>
<tr>
<td style="width: 70%;"><b>Key</b></td>
<td><b>Value</b></td>
</tr>
</thead>
<tbody>
<tr>
<td><span style="font-weight: 400;"><code>/test/foo/”hello”/4</code></span></td>
<td><span style="font-weight: 400;"><code>Ø</code></span></td>
</tr>
<tr>
<td><span style="font-weight: 400;"><code>/test/foo/”hello”/10</code></span></td>
<td><span style="font-weight: 400;"><code>Ø</code></span></td>
</tr>
</tbody>
</table>

请注意，这些键不但包含了索引列stringVal，而且包含了主键列的key 。CockroachDB会发现主键列的key，并避免对整行的不必要的查询。

最后，让我们来看看唯一索引是如何编码的。 跟之前我们创建的索引foo不同，我们将创建uniqueFoo：

    CREATE UNIQUE INDEX uniqueFoo ON test (stringVal)

不像非唯一索引，唯一索引的key中只包含索引部分的列。存储在这个key的值是所有的不属于索引的那些列的主键值的编码。 我们的test表中的两行将编码为：

<table>
<thead>
<tr>
<td style="width: 70%;"><b>Key</b></td>
<td><b>Value</b></td>
</tr>
</thead>
<tbody>
<tr>
<td><span style="font-weight: 400;"><code>/test/uniqueFoo/"hello”</code></span></td>
<td><span style="font-weight: 400;"><code>/4</code></span></td>
</tr>
<tr>
<td><span style="font-weight: 400;"><code>/test/uniqueFoo/"hello”</code></span></td>
<td><span style="font-weight: 400;"><code>/10</code></span></td>
</tr>
</tbody>
</table>

我们使用ConditionalPut写key，以检测该键是否已经存在，存在则表明违反了唯一性约束。

这就是简单的描述CockroachDB如何将SQL数据映射到key-value存储中。留意接下来我们有关查询分析，计划和执行的文章，

_在key-value存储之上实现SQL的想法并不是CockroachDB特有的。MySQL构建在InnoDB，Sqlite4和其它数据库中都是这样设计的。_

--------------------

