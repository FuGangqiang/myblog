created: 2015-09-19T20:24:26+08:00
tags: [postgresql, 数据库]


pg\_stat\_statements 模块主要用来对数据库中执行的所有 sql 语句进行一些统计分析，
以便于用来监控和分析 postgres 数据库性能。

当 pg\_stat\_statements 被加载时，数据库就生成了一个名为 `pg_stat_statements` 的视图，有以下列：

* userid
* dbid
* queryid
* query
* calls
* total\_time
* min\_time
* max\_time
* mean\_time
* stddev\_time
* rows
* shared\_blks\_hit
* shared\_blks\_read
* shared\_blks\_dirtied
* shared\_blks\_written
* local\_blks\_hit
* local\_blks\_read
* local\_blks\_dirtied
* local\_blks\_written
* temp\_blks\_read
* temp\_blks\_written
* blk\_read\_time
* blk\_write\_time


## 数据库配置

```sql
shared_preload_libraries = 'pg_stat_statements'
pg_stat_statements.max = 1000
pg_stat_statements.track = all
```


## 清除目前的 pg\_stat\_statements 的统计信息

```sql
SELECT pg_stat_statements_reset();
```


## 查询最耗时的 sql 语句

```sql
SELECT
   query,
   calls,
   total_time,
   rows,
   100.0 * shared_blks_hit / nullif(shared_blks_hit + shared_blks_read, 0) AS hit_percent
FROM pg_stat_statements
ORDER BY total_time DESC
LIMIT 5;
```
