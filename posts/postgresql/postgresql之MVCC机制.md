created: 2017-02-11T21:05:18+08:00
tags: [postgresql, 数据库]


许多数据库用到了 MVCC(Multi Version Concurrency Control，多版本并发控制)技术，
不仅保证了数据库的 ACID 特性，也提高了并发性能，本文讲解 postgresql 的 MVCC 机制。

在说明 MVCC 机制之前，我们需要了解一下事务 ID。

## 事务 ID

在 postgresql 中，每个事务都有一个唯一的事务 ID，被称为 `XID`。
当一个事务开始时，postgresql 递增 `XID`，然后把它赋给这个事务，
当前事务 `XID` 可以通过下面获取：

```sql
select txid_current();
```


## 隐藏的属性

当我们向数据库中插入一条记录时，postgresql 会自动为这条记录添加一些隐藏属性：`xmin`、`xmax`。
postgresql 就是利用这两个属性来判断这条记录是否对哪个事务可见的。

首先来说一下 `xmin`，当记录被插入时，其 `xmin` 被赋为当前事务 ID，
当事务被提交后，
其它事务会利用自己的 `XID` 和该条记录的 `xmin` 比较，
当 `XID > xmin` 时，该条记录对事务可见，否则不可见。

`xmax` 是用来标示记录失效的，记录最初被插入时，`xmax` 被赋为 0。
postgresql 中的修改并不是真正的原地修改记录，
而是新插入一条被修改后的记录，
并对原记录打标签（它的 `xmax` 会被赋为当前事务 ID），
这样，当前事务提交后，
其他事务会利用自己的 `XID` 和该条记录的 `xmax` 比较，
当 `xmax != 0 && XID > xmax` 时，该条记录对事务失效，否则可见。

删除记录同理，只是对记录打标签。

```
create table numbers (value int);
```

![xmin/xmax](/media/postgresql/mvcc_xmin_xmax.jpg)

`xmin` 和 `xmax` 的值通常会被隐藏的，但是你可以直接请求它们：

```sql
select *, xmin, xmax from tbl_name;
```


## VACUUM

从上面我们知道，当记录被修改或删除后，并没有真是的从数据库文件中移除，
而是被打上了失效标签，以便于同时期的其他事务使用，
但是当没有事务再可以访问它们时，它们就被称为 `dead rows` 了，
它们需要定期的被收集并从数据库文件中真实删除。

数据库每开启一个事务，事务 ID 就会增 1，
而存储事务的 ID 只有 32 bit，
因此当事务 ID 增加到最大值后会归 0，
突然间所有的记录都变成了发生在将来的事务所产生的，所有的新事务都没有办法访问到这些旧记录了。

针对以上问题，可以通过 `VACUUM` 命令来解决，
当然 postgresql 也提供了 auto_vacuum 守护进程来定期运行。
