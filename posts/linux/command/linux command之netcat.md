created: 2015-03-07T21:32:45+08:00
tags: [linux, CLI]


`netcat`(或 `nc`)是一个使用 `TCP` 和 `UDP` 协议来读写网络的命令工具。
顾名思义，`cat` 是 Unix/Linux 系统中通过标准输入输出用来显示文本的命令，
而 `netcat` 是将标准输入输出链接到 `TCP` 或 `UDP` socket，用来读写网络数据的命令。

`netcat` 被称为网络工具中的瑞士军刀，经常被用来调试和研究网络。


## Server-Client

`netcat` 可以很方便的模拟 socket 的 Server-Client 模式：

服务端监听 6666 端口：

```
nc -l 6666
```

客户端连接：

```
nc localhost 6666
```

这样服务端和客户端之间就可以键入任何数据进行会话了。


## IPV4 和 IPV6

`-4` 和 `-6` 选项可以强制使 `netcat` 使用何种形式的网络地址：

使用 IPV4：

```
nc -4 -l 6666
nc -4 localhost 6666
```

使用 IPV6：

```
nc -6 -l 6666
nc -6 localhost 6666
```


## TCP 和 UDP

`netcat` 默认使用 TCP 协议，但是可以利用 `-u` 参数强制使用 UDP 协议：

```
nc -4 -u -l 6666
nc -4 -u localhost 6666
```


## 端口扫描

尽管 `nmap` 更适合端口扫描，但 `netcat` 也可以做到：

```
nc -z -v -n 8.8.8.8 21-25
```

其中：

* `-z`：代表 zero IO，一旦链接上立马断开链接，不传输任何数据
* `-v`：verbose 输出
* `-n`：不使用 DNS lookup


## 传输文件

服务端发送文件：

```
nc -l 12345 < hello.c
```

客户端接收文件：

```
nc localhost 12345 > hello.c
```

当然，你也可以服务端接收文件，客户端发送文件，但是两种方式服务端必须先前于客户端启动。


## 传输文件夹

服务端发送文件夹：

```
tar -cvf – dir_name | nc -l 6666
```

客户端接收文件夹：

```
nc -n ip_addr 6666 | tar -xvf -
```


## 在线观看视频

服务端发送视频：

```
cat video.avi | nc -l 6666
```

客户端接收并观看视频：

```
nc ip_addr 6666 | mplayer -vo x11 -cache 3000 -
```


## 复制设备

服务端发送设备 raw 数据：

```
dd if=/dev/sda | nc -l 6666
```

客户端接收设备 raw 数据并写入到新设备：

```
nc -n ip_addr 6666 | dd of=/dev/sda
```

通常利用这一功能来简化安装系统。


## 获取网页

```
printf "GET / HTTP/1.0\r\n\r\n" | nc www.example.com 80
```


## 建立一个单页静态网站

```
while true
do
    { echo -ne "HTTP/1.0 200 OK\r\nContent-Length: $(wc -c < index.html)\r\n\r\n"; cat index.html; } | nc -l 6666
done
```


## 远程 shell

`ssh` 通常是用来连接远程的 shell 的，`netcat` 也是可以这样做的：

远程运行命令：

```
nc -l 6666 -e /bin/bash -i
```

本地链接：

```
nc -n ip_addr 6666
```

当然远程也可以这样做：

```
mkfifo /tmp/tmp_fifo
cat /tmp/tmp_fifo | /bin/sh -i 2>&1 | nc -l 1567 > /tmp/tmp_fifo
```

当然你也可以远程登陆 windows：

```
nc -l 6666 -e cmd.exe
```


## socat

听说比 netcat 更为强大的是 socat，不过这个没有试过。
