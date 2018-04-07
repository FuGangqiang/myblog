created: 2015-04-17T12:11:00+08:00
tags: [linux, systemd, 自动化]


linux 下有两种方式来实现定时执行任务：

* SysV init 下的 crond 后台进程
* Systemd init 下的 timer unit


## SysV init

如果你的 linux 使用 SysV init 系统，
缺省会启动 crond 进程，该进程不需要用户启动、关闭。

crond 进程负责读取调度任务并执行，用户只需要将相应的调度脚本写入 cron 的调度配置文件中。

cron 的调度文件有以下几个：

* crontab
* cron.d
* cron.daily
* cron.hourly
* cron.monthly
* cron.weekly

如果用的任务不是以 hourly、monthly、weekly 方式执行，
则可以将相应的任务写入到 crontab 或 cron.d 目录中。

如果想要看现在 crontab 正在运行什么，可以执行命令：

```
crontab -l
```

如果要编辑 crontab，可以执行命令：

```
crontab -e
```

上面命令会用默认的编辑器来让我们编辑 crontab，
如果我们修改后，保存并退出编辑器，
编辑器中的内容将会保存到 crontab 中。

crontab 中填写的格式如下：

```
* * * * * /bin/your/script.sh
```

上面格式有 6 个域组成，前 5 个都是星号（下面详解），最后一个是要执行的命令，
它的意思是每分钟执行脚本 `/bin/your/script.sh`。

crontab 中每一行前 5 个域是用来指定执行时间的，它们的含义和取值范围分别为（从前至后）：

* minute (from 0 to 59)
* hour (from 0 to 23)
* day of month (from 1 to 31)
* month (from 1 to 12)
* day of week (from 0 to 6) (0=Sunday)

如果想要每时 20 分执行一个脚本文件，crontab 添加如下：

```
20 * * * * /bin/your/script.sh
```

如果想要每天 1:30 分钟执行一个脚本文件，crontab 添加如下：

```
30 1 * * * /bin/your/script.sh
```

如果想要每个月的 2 号 1:30 执行一个脚本文件，crontab 添加如下：

```
30 1 2 * * /bin/your/script.sh
```

如果想要每年 3 月 2 号 1:30 执行一个脚本文件，crontab 添加如下：

```
30 1 2 3 * /bin/your/script.sh
```

也可以指定每星期 5 晚上：

```
30 1 * * 5 /bin/your/script.sh
```

也可以更随意一些，指定星期 1 至星期 5：

```
0 1 * * 1-5 /bin/your/script.sh
```

指定每 10 分钟执行一次：

```
0,10,20,30,40,50 * * * * /bin/your/script.sh
```

当然，也可以这样：

```
*/10 * * * * /bin/your/script.sh
```

也有一些关键字代表前五个域：

* `@reboot`：启动时执行，只执行一次
* `@yearly`： 一年执行一次，"0 0 1 1 *"
* `@annually`：与 `@yearly` 相同
* `@monthly`：一月执行一次，"0 0 1 * *"
* `@weekly`：一周执行一次，"0 0 * * 0"
* `@daily`：一天执行一次，"0 0 * * *"
* `@midnight`：与 `@daily` 相同
* `@hourly`：一小时执行一次，"0 * * * *"

crontab 可以写成这样：

```
@daily /bin/your/script.sh
```


## Systemd init

如果你的 linux 使用 Systemd init 系统，你不仅可以用 crontab 来定时执行任务，
也可以用 systemd 自身提供的 unit.timer，而且时间格式更随意，
请参考博文 [systemd 中的时间格式](/blog/posts/linux/systemd中的时间格式.html)，
其中主要讲解了 systemd 中的三种时间格式：Time Span、Timestamp、Calendar Event。

每一个 unit.timer 都必须对应另一个 unit file，unit.timer 用来描述定时器，unit file 用来描述要运行的命令。
默认 service 文件和 timer 文件同名，但是也可以在 timer 中制定 unit file 文件。

除非明确指定 `DefaultDependencies=`，否则每一个 unit.timer 默认都会把 `Conflicts=` 和 `Before=` 设定为 `shutdown.target`，
这是为了保证在系统退出前每一个 unit.timer 被停止执行。

unit.timer 一旦设置了 `OnCalendar=`，其 `After=` 就会被设置为 `timer-sync.target`，以保证在系统时钟被正确设置后运行。

每一个 unit.timer 都有一个 `[Timer]` 设置块，在这个设置块里，定义一些与定时器相关的设置：

* `OnActiveSec=`：设置计时器时间，相对于 unit.timer 被激活的时间点
* `OnBootSec=`：设置计时器时间，相对于系统启动后的时间点
* `OnStartupSec=`：设置计时器时间，相对于 systemd 被初次启动后的时间点
* `OnUnitActiveSec=`：设置计时器时间，相对于 unit.timer 中指定的 unit file 上一次被激活的时间点
* `OnUnitInactiveSec=`：设置计时器时间，相对于 unit.timer 中指定的 unit file 上一次被冻结的时间点

上面五个设置格式均为 Time Span，它们可以混合使用，比如你可以同时设置 `OnBootSec=` 和 `OnUnitActiveSec=`。

* `OnCalendar=`：设置计时器时间，格式为 Calendar Event，否则其语义与 `OnActiveSec=` 相近
* `AccuracySec=`：设置计时器精确度
* `Unit=`：设置计时器启动 unit file
* `Persistent=`：如果设置为 true，unit file 上一次被执行的时间点会被保存在硬盘文件
* `WakeSystem=`：如果设置为 true，计时器将会唤醒 suspend 状态

一个示例：

`hello.timer` 文件：

```ini
[Unit]
Description=Hello Timer Example

[Timer]
OnActiveSec=2s
OnCalendar=*-*-* *:00:00

[Install]
WantedBy=default.target
```

`hello.service` 文件：

```ini
[Unit]
Description=this is a timer example

[Service]
Type=simple
ExecStart=/your/script.sh
```
