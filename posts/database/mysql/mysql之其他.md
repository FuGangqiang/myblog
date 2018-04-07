created: 2015-02-12T21:17:47+08:00
tags: [mysql, 数据库]


# mysql 备份

* 物理备份：复制文件、文件夹，只能在同一版本、数据库
* 逻辑备份：生成 SQL 语句，不同版本、数据库迁移


### mysqldump 逻辑备份

```
mysqldump [options] db_name [tbl_name ...]
mysqldump [options] --databases db_name ...
mysqldump [options] --all-databases
mysqldump db_name > backup-file.sql

mysql db_name < backup-file.sql
```


# mysql 忘记 root 用户密码

有时数据库的登录密码忘记了，这时可以通过以下方式来重新配置数据库密码：

```
systemctl stop mysqld
mysqld_safe --skip-grant-tables &
mysql -u root
mysql> update mysql.user set password=PASSWORD('newpassword') where User='root';
mysql> flush privileges;
mysql> quit
```


# mysql 修改数据库存放位置

mysql 默认数据存放在 `/var/lib/mysql/` 目录下，
我想把这个默认目录修改到指定目录，因为我的 /var 目录所在的磁盘分区太小

```
# 停止数据库
systemctl stop  mysqld

# 创建目录,假设没有的话
mkdir /home/user/DB

# 拷贝默认数据库到新的位置
# -a 命令是将文件属性一起拷贝,否则各种问题
cp -rap /var/lib/mysql /home/user/DB/
chown mysql.mysql /home/user/DB/mysql

# 备份原来的数据
cp -a /etc/mysql/my.cnf /etc/mysql/my.cnf.bak

# 在配置文件/etc/mysql/my.cnf中的 [mysqld]项后添加 datadir 变量
[mysqld]
datadir=/home/user/DB/mysql

# 启动数据库
systemctl start mysqld
```


# mysql 远程访问设置

## 修改配置文件

在默认配置文件中：

* /etc/my.conf
* /etc/mysql/my.cnf
* SYSCONFDIR/my.cnf
* $MYSQL_HOME/my.cnf
* defaults-extra-file
* ~/my.conf

注释掉 `[mysqld]` 节中的 `skip-networking` 和 `bind-address`：

```
[mysqld]
    ...
    #skip-networking
    ...
    #bind-address = <some ip-address>
    ...
```

### 对远程用户授权


查看当前远程用户设置：

```
SELECT User, Host FROM mysql.user WHERE Host <> 'localhost';
```

对用户 `root@192.168.100.0/24` 授权访问：

```
GRANT ALL PRIVILEGES ON *.* TO 'root'@'192.168.100.%' IDENTIFIED BY 'my-new-password' WITH GRANT OPTION;
```

其中：`%` 是一个通配符。
