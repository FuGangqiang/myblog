created: 2017-02-13T16:20:21+08:00
tags: [postgresql, 数据库]


为了更好的学习 postgresql，从底层数据如何存储可以更直观的了解其原理。
本文以 postgresql9.6 为介绍对象。


## postgresql 的数据目录

在安装完数据库，我们需要用 `initdb` 命令来初始化数据库：

```sh
initdb -D /var/lib/postgres/data/
```

这样，所有数据都存放到 `-D` 指定的参数文件夹中，
当启动数据库服务时，我们需要将这个目录以参数的形式提供给启动数据库开启命令：

```sh
postgres -D /var/lib/postgres/data
```

这个目录就是在 postgresql 术语中经常碰到的 `PGDATA` 变量。

当然，有可能你是从客户端连接到数据库的，并不知道数据库开启时的命令，
你可以通过超级用户登陆 postgresql，运行下面语句来获取 `PGDATA`：

```sql
show data_directory;
```


## PGDATA 目录结构

首先我们来看一下 `PGDATA` 文件夹里面都有那些文件：

```sh
$ tree -FL 1 /var/lib/postgres/data/
/var/lib/postgres/data/
├── base/
├── global/
├── pg_clog/
├── pg_commit_ts/
├── pg_dynshmem/
├── pg_hba.conf
├── pg_ident.conf
├── pg_logical/
├── pg_multixact/
├── pg_notify/
├── pg_replslot/
├── pg_serial/
├── pg_snapshots/
├── pg_stat/
├── pg_stat_tmp/
├── pg_subtrans/
├── pg_tblspc/
├── pg_twophase/
├── PG_VERSION
├── pg_xlog/
├── postgresql.auto.conf
├── postgresql.conf
├── postmaster.opts
└── postmaster.pid

17 directories, 7 files
```

其中：

* `base/`：存储 database 数据（除了指定其他表空间的）
* `global/`：存储 cluster-wide 表格数据
* `pg_clog/`：存储事务提交的状态数据
* `pg_commit_ts/`：存储事务提交的时间戳数据
* `pg_dynshmem/`：存储动态共享内存子系统的文件
* `pg_hba.conf`：postgresql 配置文件
* `pg_ident.conf`：postgresql 配置文件
* `pg_logical/`：存储 logical decoding 状态数据
* `pg_multixact/`：存储多重事务状态数据的子目录（用于共享的行锁）
* `pg_notify/`：存储 LISTEN/NOTIFY 状态数据
* `pg_replslot/`：存储 replication slot 数据
* `pg_serial/`：存储 committed serializable transactions 信息
* `pg_snapshots/`：存储导出的 snapshots
* `pg_stat/`：存储统计子系统的永久文件
* `pg_stat_tmp/`：存储统计子系统的临时文件
* `pg_subtrans/`：存储子事务状态数据
* `pg_tblspc/`：存储指向表空间的符号链接
* `pg_twophase/`：存储 prepared transactions 的状态文件
* `PG_VERSION`：存储 postgresql 数据库的主版本号
* `pg_xlog/`：存储 WAL (Write Ahead Log) 文件
* `postgresql.auto.conf`：存储由 `ALTER SYSTEM` 设置的配置
* `postgresql.conf`：postgresql 配置文件
* `postmaster.opts`：存储上一次启动该数据库时用到的命令
* `postmaster.pid`：锁文件，只有在 postgresql 服务运行时存在，存储当前 postmaster 的 PID，PGDATA，postmaster 启动时间，端口号，Unix-domain socket 目录，第一个有效的 listen_address，共享内存的 segment ID


## database 数据存储在哪里？

postgresql 数据库中，每一个对象都对应一个 OID 唯一标识，
而每一个 database 的目录名就存储在与其对应的 OID 目录中： `PGDATA/base/oid`，
我们可以查询每一个 database 的 OID:

```sql
select oid, datname from pg_database;
  oid  |  datname
-------+-----------
 12410 | postgres
     1 | template1
 12409 | template0
 17045 | test
```

进而我们知道，与 test 数据库相关的数据都存储在 `PGDATA/base/17045` 目录里面。


## table 数据存储在哪里？

所有的 table 数据存储在所在数据库的目录里面，table 们是分开存放的，
每一个存储 table 的文件均用 `pg_class.relfilenode` 命名。

```sql
-- 创建 foo table
create table foo (
    id integer,
    content text
);


-- 查看 foo 的 relfilenode
select relfilenode from pg_class where relname = 'foo';
 relfilenode
-------------
       17238
```

因此我们就可以推断：foo 表格数据存储在 `PGDATA/base/17405/17238` 文件里面，
当然我们可以通过下面语句来查询存储相关文件：

```sql
select pg_relation_filepath('foo');
 pg_relation_filepath
----------------------
 base/17045/17238
```

为了避免有些文件系统不支持大文件，postgresql 限制标文件大小不能超过 1G，
因此，当表文件超过 1G 时，会另建一有尾缀文件 `relfilenode.1`，`relfilenode.2`，
并以此类推。

当你查看 `PGDATA/base/17405/` 目录时，
会发现有些文件命名为 `relfilenode_fsm`、`relfilenode_vm`、`relfilenode_init`，
它们是用来存储与表格相关的 free space map 、visibility map 和 unlogged table 的 initialization fork。


## row 数据是怎么存储的？

每一个 table 数据存储为一个 page 数组，每一个 page 都是固定大小（通常为 8Kb），
它们在逻辑上是等价的，因此，一个记录 row 的数据可以存放到任何一个 page 中，
一个 page 的内部结构如下：

![page structure](/media/postgresql/page_structure.png)

其中：

* `page header`: 24 字节，存储 page 的基本信息，包括 checksum、free space start/end pointer、...
* `item`：4 bytes，形为 `(offset, length)` 的二元组，指向相关 tuple
* `tuple`: 用来存储 row 的数据
* `special`：用来存储索引访问方法的特处数据，不同方法不一样，一般为空
* 空白处：未申请空间，新的 item 从其首端申请，新的 tuple 从其尾端申请

因此我们找 row 的数据需要知道哪一个 page，page 的哪一个 item，
`(page_index, item_index)`，
通常称它为 `CTID(ItemPointer)`，
我们可以通过下面语句查看每一列的 CTID：

```sql
SELECT ctid, * from foo;
```

## postgresql 限制

从上面我们知道，postgresql 中，
每一 row 的大小是不能超过 8Kb 的，因为他们是被存储在 page 里的，
因此，postgresql 限制一个表格最多有 250~1600 列（根据列类型大小而定）。

那如果列的数据超过了 8Kb 会怎么处理呢？
TOAST(The Oversized-Attribute Storage Technique)机制就是专门处理这种情况的，
它会将大数据列分割成几个小块单独存储在与表格相关的 TOAST 文件中，然后对这些小块进行索引。
