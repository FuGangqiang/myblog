created: 2017-01-12T23:24:53+08:00
tags: [security]


计算机都是通过端口(port)来进行网络通信的，可以发送数据，也可以接收数据。
有些端口默认被分配给特定的应用，比如 `80` 端口就是为了提供 `http` 服务，
而 `443` 端口默认是被用来提供 `https` 服务的，
因此可以通过禁用某些端口的发送和接收来阻止计算机的某些服务的通信，
这样可以使计算机更安全，防火墙(firewall)就是用来定义开启或关闭端口服务的规则。

iptables 和 ufw 是 linux 系统里最常见的两个防火墙应用，
而 ufw 因其易用性多用于桌面 linux 系统，本文只讲解 ufw。


## 安装

```
sudo pacman -S ufw
```


## 开机自启动 ufw

```
sudo systemctl enable ufw
```


## 添加规则

以下是设置规则的一些命令：

```
sudo ufw default allow outgoing
sudo ufw default deny incoming

sudo ufw allow ssh
sudo ufw allow 22
sudo ufw allow 80/tcp
sudo ufw allow http/tcp
sudo ufw allow 1725/udp
sudo ufw allow 1725/udp
sudo ufw allow from 123.45.67.89/24
sudo ufw allow from 123.45.67.89 to any port 22 proto tcp
```


## 删除规则

```
sudo ufw delete allow 22
```


## 检测状态

```
sudo ufw status
```


## 使规则生效

```
sudo ufw enable
```

## 开启日志

```
sudo ufw logging on
```
