created: 2015-02-10T22:42:12+08:00
tags: [mysql, 数据库]


## 安装 mysql

操作系统为 Archlinux，安装当然用 pacman 命令了。

```
pacman -S mariadb
```

## 初始化

```
mysql_install_db --user=mysql --basedir=/usr --datadir=/var/lib/mysql
```


## 启动mysql

用 systemctl 启动 mysql 后台服务进程

```
systemctl start mysqld
```

你也可以控制开机时自动启动 mysql 的后台服务进程

```
systemctl enable mysqld
```


## 设置 mysql

运行 mysql 之前要对 mysql 设置一下

```
mysql_secure_installation
```

这个命令提示你需要 root 密码，
当然首次安装 mysql 时是没有设置 root 密码的，因此按 Enter 键就好了，
接着它会提示你设置 root 密码，
再接着就是一系列选项：

* 是否允许匿名用户
* 是否允许远程登录
* 是否删除test数据库
* 是否使设置立即生效

这些选项我都是回答 `Y` 的。


## 登录 mysql

```
mysql -u username -p -h host -P port databasename
```

如果省略 `-h` 选项，默认为本机(localhost)，也可省略 databasename，但进入后需用 `use` 命令切换数据库。


## SQL测试

```sql
-- 查看MySQL的状态
status;
-- 显示支持的引擎
show engines;
-- 显示所有数据库
show databases;
-- 创建一个数据库
create database mydb default charset=utf8;
-- 切换数据库上下文,即设置当前会话的默认数据库
use mydb;
-- 显示本数据库所有的表
show tables;
-- 创建一个表
create table test (
    id int(11) unsigned not null auto_increment,
    userid char(36),
    lastlogintime timestamp,
    primary key (id)
);

describe test;

-- 插入测试数据
insert into test(userid) values ('admin'), ('haha') ;

-- 简单查询
select * from test;
select id, userid from test  where userid='admin' ;
```
