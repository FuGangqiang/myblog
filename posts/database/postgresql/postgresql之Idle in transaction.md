created: 2015-09-15T22:46:21+08:00
tags: [postgresql, 数据库]


# Idle in transaction

从 `pg_stat_activity` 中可以查询 postgres 现有的所有链接：

```
sql=> SELECT * from pg_stat_activity;
-[ RECORD 1 ]----+--------------------------------
datid            | 16385
datname          | zinn
pid              | 29006
usesysid         | 16384
usename          | zinn
application_name | psql
client_addr      | ¤
client_hostname  | ¤
client_port      | -1
backend_start    | 2016-12-10 10:52:46.889072+08
xact_start       | 2016-12-10 10:53:07.023843+08
query_start      | 2016-12-10 10:53:07.023843+08
state_change     | 2016-12-10 10:53:07.023845+08
wait_event_type  | ¤
wait_event       | ¤
state            | active
backend_xid      | ¤
backend_xmin     | 9391
query            | SELECT * from pg_stat_activity;

Time: 69.953 ms
```

如果 `state` 是 `idle in transaction`，
就表明该 postgres 后台的一个进程开启了一个事务，并做了一些事情，
然后就一直什么也不做了，既不 `commit`，也不 `rollback`。

需要 `kill` 掉相关的 `pid` 进程，或者用：

```
select pg_cancel_backend(pid);
-- 或
select pg_terminate_backend(pid);
```
