created: 2017-02-24T16:55:12+08:00
tags: [postgresql, 数据库]


数据库连接缓冲有两种方式：客户端缓冲池和服务端缓冲池，
本文讲解服务端缓冲池 PgBouncer。


## 为什么需要链接缓冲池？

postgresql 是一种 client/server 模型的数据库服务，
server 端由 master 进程(postgres)控制，
每当有客户端发起链接请求时，master 进程会 fork 一个新进程出来，
由新进程来处理客户端发起的命令查询等。
当客户端断开链接后，与之对应的进程也就被销毁，以释放相应的服务资源。

当链接请求比较多或频繁时，
服务端在进程的切换和新建上浪费了大量的资源，
所以应该尽可能减少链接请求，
也就出现了在客户端／服务端两种链接复用的方式来缓解服务端的压力。

而 PgBouncer 就是被用来在服务端一侧来复用链接的。


## 安装

archlinux 系统：

```
pacman -S pgbouncer
```


## 配置

/etc/pgbouncer/pgbouncer.ini:

```
[databases]
template1 = host=127.0.0.1 port=5432 dbname=template1

[pgbouncer]
listen_port = 6543
listen_addr = 127.0.0.1
auth_type = md5
auth_file = users.txt
logfile = pgbouncer.log
pidfile = pgbouncer.pid
admin_users = someuser
```

/etc/pgbouncer/userlist.txt:

```
"someuser" "same_password_as_in_server"
```

## 开启 PgBouncer 服务

```
systemctl start pgbouncer
```

当然，也可以直接运行 `pgbouncer`：

```
pgbouncer -d /etc/pgbouncer/pgbouncer.ini
```

postgresql 客户端程序就可以通过 `6543` 端口来申请数据库链接了。
