created: 2015-02-11T21:24:36+08:00
tags: [mysql, 数据库]


# 新建用户、组

```
use mysql
```

## 用户

```
create user username identified by 'password';
flush privileges;

drop user username;

rename user donald to 'duck'@'localhost';

create user 'joffrey'@'192.168.0.3';
create user 'joffrey'@'%';

```

## 修改密码

```
set password for 'bob'@'%.loc.gov' = password('newpass');
```


## 角色

```
create role rolename;
drop role rolename;

set role rolename;
```

## 分配权限

```
show grants [for user];

grant select on test.t1 to 'joffrey'@'192.168.0.3';
grant select on test.t2 to 'joffrey'@'%';

grant all privileges on testdb.* to username identified  by 'password';
flush privileges;

grant rolename to username;
```

## 撤销权限

```
revoke all privileges on *.* from username;
revoke pritname on testdb.* from username;
revoke rolename from username;
```
