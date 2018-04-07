created: 2014-12-13T17:34:02+08:00
tags: [linux, CLI]


## 简介

ssh 的英文全称是 Secure SHell。

ssh 协议族可以用来进行远程控制，在计算机之间传送文件。
而实现此功能的传统方式，
如 telnet（终端仿真协议）、rcp、ftp、rlogin、rsh 都是极为不安全的，
并且会使用明文传送密码。

Openssh 是 ssh(Secure SHell)协议的免费开源实现。


## 验证

在客户端来看，ssh 提供两种级别的安全验证：

* 基于密码的安全验证：知道帐号和密码，就可以登录到远程主机，并且所有传输的数据都会被加密。
* 基于密钥的安全验证:需要依靠密钥，也就是你必须为自己创建一对密钥，并把公有密钥放在需要访问的服务器上。

在客户端，基于密码的安全验证操作很简单，
只需运行 `ssh user@host` 命令，
然后根据提示输入密码就可以了，无需配置。
当远程服务器ssh的端口不是默认 22 的时候，上面命令还需指定端口号，
即 `ssh user@host -p port`。

本文主要介绍客户端基于密钥安全验证的安装和配置。


## 配置


### 创建密钥对

```
ssh-keygen -t rsa -C "your_email@host.com"
```

其中：

* `-t` 参数指明密钥的类型，默认即 rsa ，可以省略，也有其他类型。
* `-C` 设置注释文字，比如你的邮箱。

运行以上命令，一路回车（当然也可以根据提示设置），默认会在 `~/.ssh/` 目录下生成一对密钥文件，
即你的公钥文件 id\_rsa 和私钥文件 id\_rsa.pub，
公钥文件是放在远程ssh服务器上面的，而私钥文件是放在本地客户端的。

你可以生成多个不同的密钥对文件，分别用于登录不同的远程 ssh 服务器，你也可以只用一对密钥文件来登录。


### 复制公钥至远程ssh服务器

默认 ssh 端口为 22：

```
ssh-copy-id -i ~/.ssh/id_rsa.pub user@host
```

用 `-p` 参数指定 ssh 端口：

```
ssh-copy-id -i ~/.ssh/id_rsa.pub -p port user@host
```

运行以上命令后，就可以无需输入密码就可以登录远程 ssh 服务器了，如下：

```
ssh user@host
ssh -p port user@host
```

当你只用一对密钥文件来登录不同的远程 ssh 服务器时，只需把你的公钥文件分别复制到远程 ssh 服务器上就可以了。
但用多对不同的密钥文件登录设置远程登录时，可以用 ssh 的 config 文件配置。


### ssh config 配置文件

ssh 的 config 不仅可以配置多个密钥对，也可以设置快捷命令，登录时不用每次都键入用户名和远程服务器地址。

ssh 的 config 文件为 `~/.ssh/config`，以下是一个范例：

```
Host   h1
    HostName        123.45.67.89
    Port            2020
    User            user1
    IdentityFile    ~/.ssh/id_rsa_file_1

Host   h2
    Hostname        89.67.45.123
    Port            22
    User            user2
    Identityfile    ~/.ssh/id_rsa_file_2
```

当有以上配置后，就可以简单的运行如下命令进行ssh登录了：

登录到123.45.67.89

```
ssh h1
```

登录到89.67.45.123

```
ssh h2
```

同样，基于 ssh 协议的 rsync、scp、git 等命令指定远程服务器时均可以使用此简写形式。


## 端口转发


ssh 不仅可以加密网络通信，也可以用来端口转发，这一过程也被叫做“隧道”（tunneling）。


### 本地端口转发


```
ssh -L 15432:localhost:5432 user@ssh_host
```

以上命令会在本地建立一个端口 `15342`，
所有发送到本地端口 `15432` 的数据均会通过 user@ssh\_host 的 ssh 链接转发到 ssh\_host 机器上的 `5432` 端口，
这样就可以通过远程服务器的 ssh 端口访问服务器不对外开放的端口了。

postgres 的端口默认为 `5432`，如果在远程服务器开启了 postgres 服务，
但是只对本机开放而没有对外开放 `5432` 端口，我们就可以通过运行以上命令开启端口转发，
进而通过在本地运行命令访问远程服务器的 postgres：

```
psql -h localhost -p 15432 -U user dbname
```

本地的 config 文件配置如下：

```
Host postgres-tunnel
    Hostname        ssh_host
    Port            22
    User            user
    Identityfile    ~/.ssh/id_rsa
    LocalForward    15432 localhost:5432
    ServerAliveInterval 30
    ServerAliveCountMax 3
```

可以利用 autossh 自动监控 ssh 链接，如果链接断开再自动重启:

```
autossh -M 0 -fTN postgres-tunnel
```

可以设置 systemd service 文件开机自启动：

```
[Unit]
Description=AutoSSH tunnel service for postgres
After=network.target

[Service]
Environment="AUTOSSH_GATETIME=0"
ExecStart=/usr/bin/autossh \
    -M 0 \
	-o "ServerAliveInterval 30" \
	-o "ServerAliveCountMax 3" \
	-NL 15432:localhost:5432 \
	user@ssh_host

[Install]
WantedBy=multi-user.target
```


### 远程端口转发

如果本地不能 ssh 链接到远程服务器，但是远程服务器可以链接到本地，
这时我们可以在远程服务器上通过以下命令来实现端口转发：

```
ssh -R 15432:localhost:5432 user@ssh_host
```

这里的 `user@ssh_host` 对应就是本地机器的账户和地址。


### 动态端口转发


当我们在一个不安全的 WiFi 环境下上网，可能有人会窃取你的密码及隐私信息，
这样我们可以用动态端口转发来避免这种情况的发生。

```
ssh -D 10080 user@ssh_host
```

这样我们在本地 `10080` 端口建立了一个 SOCKS 代理服务，
然后我们可以设置浏览器通过本地 `10080` 端口的 SOCKS 代理来进行上网就可以了。

chromium 浏览器可以通过以下命令设置代理上网：

```
chromium --proxy-server="socks://localhost:10080"
```

### 其他参数

与端口转发配合使用的一些选项：

* `-f`: 通过验证后，fork 后台运行
* `-N`: 不执行脚本或命令
* `-C`: 压缩数据传输
* `-g`: 将本地转发端口开放给其他机器
* `-T`: 不用虚拟终端
* `-v`: 输出命令详情
