created: 2015-09-07T21:38:42+08:00
tags: [postgresql, 数据库]


默认：
```
create database mydb template template0;
```

将mydb数据库设定为模板：

```
update pg_database set datistemplate=true where datname='mydb';
```
