created: 2016-09-11T10:46:04+08:00
tags: [linux, systemd, 容器]


作为一个 systemd 重度使用者，经常用 docker 容器进行一些本地测试或搭建一个开发环境，
因为 systemd 提供了 systemd-nspawn 来模拟 chroot，无疑它也是可以用来构建容器的，
所以就摸索了一番，总结如下。


## 什么是 systemd-nspawn？

systemd-nspawn 很像 chroot 命令，但是更为强大，
它可以全面虚拟化整个文件系统、进程树、各种各样的 IPC 子系统以及主机名和域名。
它可以用来在一个轻量级的容器内运行一个命令或操作系统。

由于是基于 systemd init 系统，所以它也可以利用现有的 systemd 各种组件命令。


## 构建一个最小的 Archlinux 容器

因为我本机是 Archlinux，所以就以构建 Archlinux 容器为例了，首先创建一个容器的顶层文件夹：

```
mkdir -p ~/Containers/arch
```

利用 pacstrap 将 Archlinux 基本系统安装进容器文件夹：

```
pacstrap -c -d ~/Containers/arch base
```

因为容器和宿主机可以共享 linux 内核，所以初始化容器文件夹可以忽略 linux 包：

```
# pacstrap -i -c -d ~/Containers/arch base --ignore linux
==> Creating install root at arch
==> Installing packages to arch
:: Synchronizing package databases...
 core                                        120.1 KiB   619K/s 00:00 [######################################] 100%
 extra                                      1755.6 KiB  1600K/s 00:01 [######################################] 100%
 community                                     3.6 MiB  2.90M/s 00:01 [######################################] 100%
:: linux is in IgnorePkg/IgnoreGroup. Install anyway? [Y/n] n
```

其中 `-i` 选项避免自动确认。

这样一个 Archlinux 容器就构建成功了，你可以通过 systemd-nspawn 命令开启容器：

```
systemd-nspawn -b -D ~/Containers/arch -n
```

容器启动后，可以用 root 用户名登陆，无须密码，进入容器系统后，你就可以搭建自己的容器环境了。

当然，你也可以很容易的构建一个 debian 或 fedora 容器，网上有许多构建方法，这里不再讲解。


## machinectl

systemd-nspawn 开启的容器被称为 machine，可以利用 systemd 中的 machinectl 命令进行各种容器操作。

machinectl 命令默认会到 `/var/lib/machines`、`/usr/local/lib/machines/` 和 `/usr/lib/machines/` 目录搜索容器。
为了可以用 machinectl 命令，我们将上面的容器文件夹移动到 `/var/lib/machines/arch` 文件夹下：

```
mv ~/Containers/arch /var/lib/machines/arch
```

因为 archlinux 中 pam_security 的控制，machinectl 命令是不能 root 登陆的，为了解决这个问题，需要在容器里修改文件
`/etc/securetty`，添加 `pts/0`。

这样我们就可以用 machinectl 来控制我们的 `arch` 容器了。

开启 `arch` 容器：

```
machinectl start arch
```

登录 `arch` 容器：

```
machinectl login arch
```

运行 `arch` 容器命令：

```
machinectl shell arch /usr/bin/pwd
```

关闭 `arch` 容器：

```
machinectl poweroff arch
```

machinectl 其他一些命令：

```
machinectl reboot container_name
machinectl terminate container_name
machinectl kill container_name

machinectl list
machinectl status container_name
machinectl show container_name
machinectl enable container_name
machinectl disable container_name

machinectl bind container_name PATH [PATH]
machinectl copy-to container_name PATH [PATH]
machinectl copy-from container_name PATH [PATH]
```

machinectl 也有很多与容器 image 相关的命令，这里不再介绍。


## 与其他 systemd 组件配合使用

systemd 系列组件大多都有一个 `-M` 选项，此选项就是用来指定容器，容器搜索路径与 machinectl 一样。

我们可以这样用 systemd-nspawn 来开启容器：

```
systemd-nspawn -M container_name
```

查看容器日志：

```
journalctl -M container_name
```

查看容器进程控制组内容：

```
systemd-cgls -M container_name
```

分析容器启动过程：

```
systemd-analyze -M container_name
```


## 开机自启动指定容器


```
systemctl enable systemd-nspawn@container_name.service
```
