created: 2015-09-05T20:12:43+08:00
tags: [postgresql, 数据库]

在 postgres 8.0 版本之后，用户(user)和用户组(group)的生成被 `create role` 语句统一起来了，
但是为了兼容，也保留了 `create user` 和 `create group` 语句。

当 initdb 后，postgres 默认生成了一个超级 role 用户：postgres，
在操作数据库之前应该用 postgres 登录数据库，为数据库建立一些用户和用户组。


## 用户(user role)

```
create role fu login password 'abcdefg' createdb;
```

以上语句生成了一个名为 `fu` 的用户，可以用这个用户名登录，登录密码为 `abcdefg`，这个用户可以自行建立 database。

用户名默认一直有效，但也可以为用户名指定有效期：

```
# 默认设置
create role fu login password 'abcdefg' createdb valid until 'infinity';

# 制定有效期
create role fu login password 'abcdefg' createdb valid until '2050-05-05 05:05';
```


## 用户组(group role)

用户组通常是那些没有 login 权限，但是有其他 role 作为成员的 role，
当然，这只是一个约定，不是强制的，你也可以创建一个既能登录也包含其他 role 成员的用户组。

```
# 生成一个group role
create role fugroup inherit;

# 添加其他用户或组
grant fugroup to fu;
```
