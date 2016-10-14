date: 2016-01-30 12:47:53
tags: git


## 服务器

本文示例中服务器 ip 地址：192.168.0.1


## 创建 git 用户

在服务器上创建一个提供 git 服务的用户,这里以 `git` 为例:

```
useradd git
passwd git
```


## 创建 git 仓库

在服务器上创建一个 git 仓库:


```
su git
mkdir test.git
git --bare init
```


## 添加 ssh 公钥到服务器

项目组中的每一名成员在自己的机器上运行:

```
ssh-copy-id -i ~/.ssh/id_rsa.pub git@192.168.0.1
```


## 禁止项目成员登陆服务器进行其他操作

在服务器中修改 `/etc/passwd` 中 `git` 用户的 `shell` 项为 `/usr/bin/git-shell`(这里应该是 `which git-shell` 的结果)


## 复制 git 仓库

项目成员可以复制服务器中的 git 仓库了:

```
git clone git@192.168.0.1:test.git
```
