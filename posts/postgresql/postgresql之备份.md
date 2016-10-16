date: 2016-10-15 16:51:19
tags: postgresql, 数据库


postgresql 数据备份有三种方式：

* sql 转储(sql dump)
* 冷备份(file system level backup)
* 热备份(continuous archiving)


## sql 转储

sql 转储不仅可以在 postgresql 的不同版本间恢复，
而且可以在不同的体系架构恢复（比如从 32 位转移到 62 位），
而其他的两种备份方法是不能这样的。

sql 转储的版本是 `pg_dump` 开始运行时的数据库快照，
在运行 `pg_dump` 时并不会阻塞其它数据库操作，
快照的数据均是相对于 `template0` 模板的。


```
# 备份
pg_dump -h host -p port -U user -n schemaname -t tablename dbname > data.bak

# 恢复
createdb dbname
psql -h host -U user -d dbname < data.bak
```

运行 `psql` 时，目标 dbname 必须被创建，并且数据库中相关的用户及权限也必须存在。

默认 `psql` 如果遇到错误仍会继续执行，如果想要遇到错误停止运行：

```
psql --set ON_ERROR_STOP=on dbname < data.bak
```

如果想要在一个事务中执行：

```
psql -1 dbname < data.bak
psql --single-transaction dbname < data.bak
```


`pg_dump` 一次只能处理一个数据库，而且不包含 `role` 和 `tablespace` 的定义，
而 `pg_dumpall` 命令可以备份整个数据库集，而且也包括 `role` 和 `tablespace` 的定义。

```
# 备份
pg_dumpall > data.bak

# 恢复
psql -f data.bak postgres
```

如果想要把所有数据分开备份，可以组合使用 `pg_dumpall --globals-only` 和 `pg_dump` 命令。

当备份的数据库很大时，你也可以对输出文件进行压缩：

```
pg_dump dbname | gzip > data.bak.gz
gunzip -c data.bak.gz | psql dbname
```

也可以使用 split 命令：

```
pg_dump dbname | split -b 1m - data.bak
cat data.bak* | psql dbname
```

如果想要更灵活的备份数据库，可以用 `pg_dump -F` 来制定输出格式：

```
pg_dump -Fc dbname > data.bak
pg_restore -d dbname data.bak
```

可以利用 `pg_dump -j` 来加速备份数据库：

```
pg_dump -j num -F d -f out.dir dbname
```


## 冷备份

冷备份就是把在 `PGDATA` 文件夹下的所有相关数据复制下来，
但是这种方法有一个缺点，就是在备份或恢复时 postgresql 相关进程服务都必须停止下来，
因此所有的数据库链接都会被关闭。

```
# 备份
systemctl stop postgresql
tar –jcvf backup.tar.bz2 $PGDATA

# 恢复
tar –jxvf backup.tar.bz2
systemctl start postgresql
```

如果你的文件系统支持快照(snapshot)特性，也可以利用这个特性来冷备份，
这种方法无需停止 postgresql 相关进程服务，
但是这种备份方法在恢复时应该像数据库宕机一样处理，重新执行 WAL 日志文件。


## 热备份

当数据库比较大时，可以利用热备份，也就是增量备份。

### 将数据库切换为归档模式

postgresql 默认处于非归档模式，修改 `PGDATA/postgresql.conf` 配置文件开启归档模式：

```
wal_level = archive
archive_mode = on
archive_command = 'test ! -f /mnt/server/archivedir/%f && cp %p /mnt/server/archivedir/%f'
# archive_command = 'ssh arc_svr test ! -f /path/to/archive/%f && scp %p arc_svr:/path/to/archive/%f'
```

重启 postgresql：

```
systemctl restart postgresql
```

这样数据库就处于归档模式下了：

```sql
select name, setting from pg_catalog.pg_settings where name like 'archive%' or name = 'wal_level';
```


### 对数据库进行基础备份

进行基础备份有两种方法：

* pg_basebackup
* pg\_start\_backup


#### 利用 pg_basebackup 进行数据库基础备份

`pg_basebackup` 使用复制协议，因此需要配置 `PGDATA/pg_hba.conf` 文件以允许 replication 连接：

```
local   replication     postgres                                peer
host    replication     postgres        192.168.0.0/24          md5
```

修改 `PGDATA/postgresql.conf` 选项 `max_wal_senders` 以允许至少一个 session 连接来进行备份：

```
max_wal_senders = 1
```

重启 postgresql：

```
systemctl restart postgresql
```

复制基础备份目录：

```
sudo -u postgres pg_basebackup -RPv　-D  back_backup_20161015
```

这会生成一个备份目录，其目录结构与 `PGDATA` 目录结构一致。如果想要压缩目录：

```
sudo -u postgres sh -c 'pg_basebackup -RPv -Ft -D - | bzip2 > base_backup_20161015.tbz2'
```


#### 利用 pg\_start\_backup 进行数据库基础备份


使用超级用户执行 `pg_start_backup`：

```sql
select pg_start_backup('base_backup_20161015);
```

不需要停止数据库，使用文件系统备份工具备份 `PGDATA` 目录下的数据库文件：

```
sudo -u postgres tar cjvf back_backup_20161015.tbz2 -P \
    --exclude=$PGDATA/postmaster.pid \
    --exclude=$PGDATA/postmaster.opts \
    --exclude=$PGDATA/pg_xlog \
    --exclude=$PGDATA/pg_replslot \
    --warning=no-file-changed \
	--warning=no-file-removed \
    $PGDATA
```

使用超级用户执行 `pg_start_backup`：

```sql
select pg_stop_backup();
```

`pg_stop_backup` 会将备份期间活动的 WAL 日志文件归档，
最后生成一个 `.backup` 文件标识出保证此次备份完整性所需要的最后一个 WAL 日志，
使用此次基础备份恢复系统时，不再需要之前的 WAL 日志。


### 恢复数据库

将基础备份文件目录迁移到 `PGDATA`：

```
mv $PGDATA $PGDATA.bak
mv back_backup_20161015 $PGDATA
```

创建 `PGDATA/recovery.conf` 文件：

```
restore_command = 'cp /mnt/server/archivedir/%f %p'
```

如果想要恢复到某个时间点：

```
recovery_target_time = '2016-10-17 04:00:00.0'
```

重启 postgresql:

```
systemctl restart postgresql
```

当恢复完成后，`PGDATA/recovery.conf` 会被 postgresql 数据库自动重命名为 `PGDATA/recovery.done`。
