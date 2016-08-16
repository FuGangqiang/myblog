date: 2015-01-17 20:21:00
tags: linux, systemd


现在已经有好多 Linux 发行版采用 systemd 作为它们的 init 系统了，
自己在网上看到关于 systemd 好多文章，
本文是我对它们的一个总结。


## 什么是 init?

在 Linux 中，init 就是 initialization（初始化）的缩写。
init 是一个 daemon（后台）进程，
它是 Linux 系统开机启动（内核加载完毕）后运行的第一个进程，进程号(pid)为 1，
直到系统关机它才终止运行，也就是说它也是 Linux 系统关机前运行的最后一个进程。

Linux 系统中所有其他进程都是直接或间接由 init 进程启动的，
因此 init 进程是其他所有进程的父进程或祖先进程。
因此它可以做许多其他进程不能做的事情，接管其他进程不负责的功能。

正是由于其特殊性，
万一 init 因为某种原因不能启动，那么 Linux 系统就再也无法启动其他进程，
系统就会处于“Kernel Panic”状态。

Linux的 init 系统有好多种，本文只是介绍 systemd。


## systemd 是什么？

systemd 是 System Management Daemon 的简写
（在 UNIX 系统中，后台进程都按照惯例以 d 结尾，因此当你看到一个进程的名字以 d 结尾，那它极有可能是一个后台进程），
它是 Linux 系统中启动的第一个进程，也就上面所说的 init daemon 进程。


## systemd unit

系统启动过程中，systemd 用 unit 来组织那些各种不同的任务，
例如生成网络端口、配置硬件设备、加载存储设备、开启后台服务进程、等等

systemd 要求每一个任务对应一个 unit，
而每一个 unit 都需要一个包含必要信息的配置文件，
而这些配置文件的语法很简单，
这也是 systemd 的要实现的目的之一。

unit 的类型有：

* service
* target
* path
* timer
* socket
* automount
* device
* mount
* scope
* slice
* snapshot
* swap

systemd 通过配置文件的后缀名来判断 unit 的类型的，比如一个 service 类型的 unit 的配置文件名通常类似于 name.service，
一个 mount 类型的 unit 的配置文件名通常类似于 name.mount。

这些配置文件通常存放在 /etc/systemd/system/ 和 /usr/lib/systemd/system/ 文件夹中，如果同一个配置文件名都处于这两个文件夹中，
systemd 会忽略 /usr/lib/systemd/system/ 中的同名配置文件。

systemd 会自动生成一些 unit，而这些 unit 并不会存在配置文件，但是它们可以通过 systemctl 来访问。


## 开机启动过程

systemd 最基本的任务是管理开机启动过程，并提供开机启动的一些信息。

想得到开机启动时间，运行下面命令：

```
systemd-analyze
```

想得到开机时每一个任务启动的时间，运行下面命令：

```
systemd-analyze blame
```

想得到更多的启动信息，systemd-analyze 这个命令也可以通过以下命令生成一个描述各个启动任务信息的 svg 格式图片：

```
systemd-analyze plot > plot.svg
```

想了解开机启动的每一个任务之间的依赖关系，运行以下命令：

```
systemctl list-dependencies
```

想知道哪些服务启动失败，运行一下命令：

```
systemctl --failed
```

systemd 中没有 runlevel 的概念，但是提供相似的 target 的功能：

* runlevel0  ->  runlevel0.target  ==  poweroff.target
* runlevel1  ->  runlevel1.target  ==  rescue.target
* runlevel2  ->  runlevel2.target  ==  multi-user.target
* runlevel3  ->  runlevel3.target  ==  multi-user.target
* runlevel4  ->  runlevel4.target  ==  multi-user.target
* runlevel5  ->  runlevel5.target  ==  graphical.target
* runlevel6  ->  runlevel6.target  ==  reboot.target

与 target/runlevel 相关的命令：

```
systemctl get-default
systemctl set-default name.target

systemctl isolate name.target

systemctl list-units --type target

systemctl rescue
systemctl halt
systemctl reboot
systemctl poweroff
systemctl suspend
systemctl hybrid-sleep
systemctl emergency
systemctl hibernate
```

与 unit 相关的命令：

```
systemctl list-units
systemctl list-units --type service --all --state=inactive
systemctl list-unit-files
systemctl status name.service

systemctl start name.service
systemctl stop name.service
systemctl restart name.service
systemctl try-restart name.service
systemctl reload name.service
systemctl is-active name.service

systemctl enable name.service
systemctl disable name.service
systemctl is-enabled name.service

systemctl mask name.service
systemctl unmask name.service

systemctl cat name.service
systemctl edit name.service
systemctl show name.service
systemctl show name.service -p property

systemctl list-dependencies
systemctl daemon-reload

systemctl kill httpd
systemctl set-property httpd.service CPUShares=500
systemctl show -p CPUShares httpd.service
```

通过 ssh 远程控制主机，运行以下命令：

```
systemctl --host user_name@host_name command
systemctl -H user_name@host_name command
```

## journalctl

与日志相关命令：

```
journalctl
journalctl --utc

journalctl --list-boots
journalctl -b  # since recent boot
journalctl -b -1  # last boot
journalctl -b caf0524a1d394ce0bdbcff75b94444fe

journalctl --since=today
journalctl --since "2015-06-01 01:00:00"
journalctl --since "2015-06-01" --until "2015-06-13 15:00"
journalctl --since 09:00 --until "1 hour ago"

journalctl -u nginx.service
journalctl -u nginx.service -u postgresql.service --since today

journalctl _PID=8088
journalctl _UID=33
journalctl _GID=33
man systemd.journal-fields
journalctl -F _GID

journalctl /sbin/crond
journalctl `which crond`

journalctl -k     # kernel messages, dmesg out

journalctl -p err
-p:
   * emerg
   * alert
   * crit
   * err
   * warning
   * notice
   * info
   * debug

journalctl --no-full
journalclt --no-pager

journalctl -b -u nginx.service -o json
journalctl -b -u nginx -o json-pretty
format:
      * cat
      * export
      * json
      * json-pretty
      * json-sse
      * short
      * short-iso
      * short-monotonic
      * short-precise
      * verbose

journalctl -f     # Follow messages
journalctl -n 20

journalctl --disk-usage
journalctl --vacuum-size=1G
journalctl --vacuum-time=1years
```


## hostnamectl

与 hostname 相关命令：

```
hostnamectl
hostnamectl set-hostname <hostname>
```


## localectl

与 locale 相关命令：

```
localectl
localectl status
ocalectl list-locales
localectl set-locale LANG=en_GB.utf8
localectl list-keymaps
localectl set-keymap en_GB
localectl set-x11-keymap en_GB
```


## timedatectl

与日期、时间相关命令：

```
timedatectl
timedatectl status
timedatectl set-time YYYY-MM-DD
timedatectl set-time HH:MM:SS
timedatectl set-time 'YYYY-MM-DD HH:MM:SS'
timedatectl list-timezones
timedatectl set-timezone America/New_York
timedatectl set-local-rtc boolean   # yes/no
timedatectl set-ntp boolean
```


## loginctl

与登陆相关命令：

```
loginctl list-users
loginctl list-sessions
loginctl show-user tom
```


## bootctl

与系统加载相关命令：

```
bootctl status
bootctl update
bootctl install
bootctl remove
```


## other

```
busctl
machinectl
networkctl
systemd-cgtop
systemd-cgls
```
