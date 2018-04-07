created: 2015-09-03T15:53:16+08:00
tags: [postgresql, 数据库]

## 安装 postgresql

操作系统为 Archlinux

```
pacman -S postgresql
```

源码安装：

```
./configure
make
su
make install
```


## 生成 postgresql cluster

```
adduser postgres
mkdir -p /var/lib/postgres/data
chown -c -R postgres:postgres /var/lib/postgres
su - postgres
initdb -E UTF8 --locale zh_CN.UTF-8 -D '/var/lib/postgres/data'
```

最后一步也可用 pg_ctl 命令：

```
pg_ctl -D /var/lib/postgres/data initdb
```


## 开启 postgresql

systemd 开启：

```
systemctl start postgresql
```

postgres 命令：

```
postgres -D /var/lib/postgres/data >logfile 2>&1 &
```

也可用 pg_ctl 命令：

```
pg_ctl start -D /var/lib/postgres/data -l logfile
```


## 关闭 postgresql

systemd 关闭：

```
systemctl stop postgresql
```

pg_ctl 命令：

```
pg_ctl stop -D /var/lib/postgres/data [-m s[mart] | f[ast] | i[mmediate]]
```

kill 命令：

* SIGTERM：Smart Shutdown，服务进程不再接受新的连接，但是已经存在的连接让它们正常退出
* SIGINT：Fast Shutdown，服务进程不再接受新的链接，发送 SIGTREM 信号至所有子进程，等待所有进程退出后在退出
* SIGQUIT：Immediate Shutdown，服务进程发送 SIGQUIT 至所有子进程，等待子进程推出，如果子进程没有在5秒钟内退出，发送 SIGKILL 信号至子进程

```
kill -INT `head -1 /var/lib/postgres/data/postmaster.pid`
```


## 开机自启动 postgresql


```
systemctl enable postgresql
```


## 生成 postgresql 用户及数据库

```
createuser --interactive
createdb testDB
```


## 特殊变量

* `PGDATA`: 对应 `-D` 参数
* `PGDATABASE`: 对应 `-d` 参数
* `PGHOST`：对应 `-h` 参数
* `PGPORT`: 对应 `-p` 参数
* `PGUSER`: 对应 `-U` 参数


## postgresql 数据库结构

* cluster: database 的集合
* database：schema 的集合
* schema：table、function 等的集合


## 测试

```
systemctl start postgresql
psql -U username -d testDB
```

运行:

```
create table test(
    id serial,
    name varchar(15)
);
insert into test(name) values('abc');
```


## 配置远程登陆数据库

修改配置文件 `/var/lib/postgres/data/postgresql.conf`：

```
listen_addresses = '*'
```

修改配置文件 `/var/lib/postgres/data/pg_hba.conf`：

```
host   all   all   my_remote_ip_address/32   md5
```


## 忘记登陆密码

```
su - postgres psql

sql> alter role username password 'string';
```
