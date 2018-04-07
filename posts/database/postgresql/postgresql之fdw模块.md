created: 2016-04-20T19:56:12+08:00
tags: [postgresql, 数据库]


postgres-fdw(foreign data wrapper) 模块可以让 postgres 数据库访问并更新其他 postgres 数据库。

使用方法如下：


## 加载 postgres-fdw 模块

```sql
CREATE EXTENSION　postgres_fdw;
```


## 创建 foreign server 对象

```sql
CREATE SERVER foreign_server
    FOREIGN DATA WRAPPER postgres_fdw
    OPTIONS (host '192.168.0.1', port '5432', dbname 'foreign_db');
```


## 创建 user mapping

```sql
CREATE USER MAPPING FOR local_user
    SERVER foreign_server
    OPTIONS (user 'foreign_user', password 'password');
```


## 创建 foreign table

```sql
CREATE FOREIGN TABLE foreign_table (
    id integer NOT NULL,
    data text
)
    SERVER foreign_server
    OPTIONS (schema_name 'some_schema', table_name 'some_table');
```

或者

```sql
IMPORT FOREIGN SCHEMA remote_schema
    FROM SERVER foreign_server
	INTO local_schema;
```


## 涉及的系统表：

* pg\_extension;
* pg\_foreign\_data\_wrapper;
* pg\_foreign\_server;
* pg\_foreign\_table;


## 清除 foreign data wrapper

```sql
drop foreign table foreign_table;
drop user mapping for local_user server foreign_server;
drop server foreign_server;
drop extension postgres_fdw;
```
