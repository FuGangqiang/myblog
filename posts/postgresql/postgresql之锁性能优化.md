created: 2016-10-12T11:11:07+08:00
tags: [postgresql, 数据库, 性能]


转载自：[更高的并发：改进PostgreSQL锁机制](http://www.csdn.net/article/2015-11-07/2826143)<br>

如果你想构建一个大规模的网站，单凭横向扩展 web 服务器是远远不够的。
如何巧妙地管理数据库也是非常必要的。
锁（locking）便是实现网站高扩展性的一个关键。

在 postgresql 中，借助于并发性的改进，通过减少锁及加速执行得到若干令人满意的特性。

一般推荐的做法是：
在解决锁问题之前，无论如何先要检查出在你的 postgresql 数据库服务器上正在运行的是什么，这非常有必要。
我建议参考 pg\_stat\_statements 并仔细地检查系统瓶颈（bottleneck）。


## 改进 select for update 语句

假设两人同时试图修改数据库中同一行的内容，每个用户会首先选择（select）一行来检查它的内容，然后开始更新。
令人讨厌的事情是：这两个用户很可能会找到原来的行并且覆盖彼此做的改变。
这是一个经典的竞态事件。

在现实生活中，这样会导致恶劣的后果：
例如两个人也许会预定了同一架飞机的同一个航班；
或者取款时取出的钱可能会比帐户中实际的数额更多。
这显然不是我们想要的。

再拿前面的航班机票预定为例，假设有人想要预定飞机的某一座位：

```sql
select ...
from table
where class = 'economy' and empty = true
limit 1
for update
```

现在的麻烦是：
如果另外一个人也试图抢占一个座位，他会发现该座位已经被第一个人选择。
但是，这一行是被锁定的。
第二个人的 `select for update` 操作必须等到第一个人的事务处理完成。
值得提醒的是，乘客可能非常乐意接受该航班上的其它任意座位，所以没有必要等待某个特定的座位。

postgresql 9.5 将会解决这一问题。
下面是一种新的读取行的方式：

```sql
select ...
from table
where class = 'economy' and empty = true
limit 1
for update skip locked
```

这里的巧妙之处在于 postgresql 将会简单地忽略被锁定的行，并返回一个没有被别人锁定的行。
这样是非常有意义的，因为 100 个同时在查看一个免费座位的用户会得到 100 个不同的行。
这样的结果是你没有死守一个 cpu，而是巧妙地横向扩展了系统中的所有 cpu。
由于冲突不再发生，没有人必须等其他人。


## select for share

还有一种可以使 postgresql 提供更高并发的方法。
看下面的例子：

```
select *
from account as a, currency as c
where a.currency = c.id and a.account = 4711
for update
```

在这个例子中，某人想查看他的银行账户。
现在主要的问题是：哪些行是被锁定的？
答案是：account 和 currency。

仅仅因为一个人想从 ATM 中取钱而锁定整个 currency 表显然并不是个好办法，
而应该让很多人可以同时取钱，在这一问题上，postgresql 的解决方法是提前告知需要更新哪张表。

方法很简单：

```sql
select *
from account as a, currency as c
where a.currency = c.id and a.account = 4711
for update of account for share of currency
```

通过告诉 postgresql 我们要做的事情，postgresql 数据库会在 currency 表上使用一个无害的锁。
这样大部分人可以同时查看相同的 currency 而无需相互锁定，同时又保证了 account 表的安全。


原文：[More Concurrency: Improved Locking In PostgreSQL](http://highscalability.com/blog/2015/10/13/more-concurrency-improved-locking-in-postgresql.html)
