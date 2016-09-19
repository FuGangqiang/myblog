date: 2016-09-18 17:42:59
tags: postgresql, 数据库


## 什么是执行计划(Execution Plan)？

SQL 是典型的描述性语言，也就是说用户在使用 SQL 时，只需提出什么样的计算需要执行，而不是写出如何来执行这些计算，
其更强调 What 而不是 How，例如：

```sql
select email from users where name = 'foo';
```

上面 SQL 语句描述的意思是：我们需要到 users 表查询所有 name 是 foo 的 email，只是描述了我们需要的结果。
我们写这条语句时，并不需要知道数据库是如何执行这条语句的，
比如执行这条语句的其中一个方法是：

1. 打开存储 users 表的文件
1. 读取所有行
1. 比较每一行 name 字段与 foo 字符串，如果相等将该行中的 email 字段保存到结果中
1. 返回所有结果

数据库系统将这些如何执行语句的动作全部隐藏起来了，这样就使用户可以很轻松的描述自己的问题，
但是这样做并不完美，当遇到性能问题时，如何高效的执行 SQL 语句的责任就全部丢给数据库后台了，
使数据库系统更加复杂。

数据库系统的查询优化器根据每一步骤花费的时间来选择一个最优的**执行计划**(Execution Plan)，
然后按照这个执行计划来一步一步的执行，进而获取 SQL 语句的最终结果。

这样，当我们遇到某一个效率低下的 SQL 语句，最先做的事情就是查找这个 SQL 语句的**执行计划**。


## 获取执行计划(Execution Plan)

postgresql 在 SQL 语句前面添加 `EXPLAIN` 关键字就可以获取这条 SQL 语句的执行计划：

```sql
EXPLAIN SELECT * FROM users;
```

结果如下：

```sql
                        QUERY PLAN
----------------------------------------------------------
 Seq Scan on users  (cost=0.00..5.24 rows=234 width=41)
```

在上面的例子中，EXPLAIN 并没有执行 SELECT 语句，只是对这条语句进行预测，并将预测结果(QUERY PLAN)打印出来。

我们可以利用 ANALYZE 关键字来真实的执行 SQL 语句，并打印更详细的信息：

```sql
EXPLAIN ANALYZE SELECT * FROM users;

                        QUERY PLAN
-----------------------------------------------------------
Seq Scan on users (cost=0.00..5.21 rows=173 width=118)
         (actual time=0.018..0.018 rows=0 loops=1)
 Total runtime: 0.020 ms
```

EXPLAIN 通常用来查看 SELECT 语句，但是它也可以查看 INSERT、UPDATE、DELETE、EXECUTE 或 DECLARE 语句。
注意当使用 ANALYZE 关键字时，这些语句是会被真实的执行，并改变数据库中的数据，可以使用事务命令来避免修改数据库：

```sql
BEGIN;
EXPLAIN ANALYZE ...;
ROLLBACK;
```

## 如何看懂执行计划(Execution Plan)

数据库执行 SQL 语句的步骤是这样的：

1. 读取 SQL 语句
1. 对其进行语法分析，形成一个语法分析树（或者关系代数表达式，两者相通）
1. 对语法分析树根据关系代数等式进行优化转换
1. 优化器根据一些统计数据得出一个最优的执行计划
1. 计算引擎根据执行计划计算 SQL 语句结果

因为第 4 步优化器根据语法分析树计算执行计划，所以一般执行计划也是一个树结构，
而 postgresql 的 EXPLAIN 的输出结果是文字形式，所以就会用缩进来表达这种树结构。

如下：

```sql
EXPLAIN SELECT *
FROM tenk1 t1, onek t2
WHERE t1.unique1 < 100 AND t1.unique2 = t2.unique2;

                                        QUERY PLAN
------------------------------------------------------------------------------------------
 Merge Join  (cost=198.11..268.19 rows=10 width=488)
   Merge Cond: (t1.unique2 = t2.unique2)
   ->  Index Scan using tenk1_unique2 on tenk1 t1  (cost=0.29..656.28 rows=101 width=244)
         Filter: (unique1 < 100)
   ->  Sort  (cost=197.83..200.33 rows=1000 width=244)
         Sort Key: t2.unique2
         ->  Seq Scan on onek t2  (cost=0.00..148.00 rows=1000 width=244)
```

从上面可以看出，执行计划的结构是一个计划节点树，叶子节点为 Scan 节点，
如果查询语句还有 join、aggregation、sorting等其他操作，会在这些 Scan 节点之上再添加节点来执行这些操作。

每一个计划节点都会有四个数值，比如根结点的`(cost=198.11..268.19 rows=10 width=488)`，它们分别代表：

* 执行该节点开始时间(`198.11`)
* 执行该节点消耗总时间(`268.19`)
* 执行该节点后返回行数(`10`)
* 执行该节点返回每一行的平均总字节数(`488`)

以下是最常见的几种节点：

#### Scan Methods

* Sequential Scan：扫描整个 table 选择结果
* Index Scan：从 B-tree 索引筛选，然后根据索引扫描 table 获取结果
* Index Only Scan：同 Index Scan，只是不需要再次查询 table 了，所有筛选和结果都使用索引
* Bitmap Index Scan：对普通的 Index Scan 的优化，从 B-tree 索引中筛选的结果首先保存在一个 bitmap 中，
  然后对这个 bitmap 按照在磁盘中存储顺序排序，再按照这个顺序查询 table。


#### Join Methods

* Nested Loop：最通用的双循环合并
* Hash Join：将较小的 table 放到一个 hash 表中，然后扫描较大的 table 并按照合并条件合并
* Merge Join：将两个 table 分别排序，然后根据合并条件合并
