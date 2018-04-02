created: 2015-03-29T12:46:00+08:00
tags: [python, network]


UDP(User Datagram Protocol，用户数据报传输协议)位于 TCP/IP 协议的传输层，
比 TCP 简单许多，号称无连接不可靠传输协议。


传送数据不需要建立连接，只须知道对方的 IP 地址和端口号，
就可以直接发送数据包，但是因为其直接建立在不可靠的 IP 协议之上，对方能不能收到就不知道了，
在网络质量不好的情况下，使用 UDP 协议时丢包现象十分严重。

尽管有这些缺点，它也有 TCP 没有的优点，它简单速度快，资源占用少，也可以子网广播，
对于不要求可靠到达的数据，就可以使用 UDP 协议，比如视频、音频的传输。

由于 UDP 的特点，它也非常适合那些只发送一次字节占较小的请求，然后就完成工作，不再继续进行发送或接受数据的操作。比如 DNS 协议就是基于 UDP 的。


## socket 简单版


### client 端

1. 建立 UDP socket 对象
2. 直接发送信息到指定地址
3. 接收服务端回信

示例如下：

```python
#!/usr/bin/env python

import socket

ADDR = ('127.0.0.1', 6666)
MAX = 65535
MESSAGE = 'hello world!'

s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
print('Send to server:', MESSAGE)
s.sendto(MESSAGE.encode('utf-8'), ADDR)
data, addr = s.recvfrom(MAX)
print('The Server reply:', data.decode('utf-8'))
```


## server 端

1. 建立 UDP socket 对象
2. 绑定端口、地址
3. 循环接受、回复客户端信息

示例如下：

```python
#!/usr/bin/env python

import socket

ADDR = ('127.0.0.1', 6666)
MAX = 65535

s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.bind(ADDR)
while True:
    data, addr = s.recvfrom(MAX)
    print('the client at', addr, 'say:', data.decode('utf-8'))
    replay = 'Your data was {} bytes'.format(len(data))
    s.sendto(replay.encode('utf-8'), addr)
```

## socket 改进版

由于 UDP 是不可靠传输协议，当客户端向服务端发送数据后，有可能发生以下情况：

* 数据在传输过程中丢失，没有到达服务端
* 数据达到服务端，服务端回复信息在传输过程丢失
* 服务端进程没有运行，客户端发送消息不会被处理和回复
* 数据由于网络速度过慢不能及时收到回复

上面前三种情况，都会使简单版客户端就会发生阻塞(recvfrom 函数)进而导致程序不会终止，
我们需要一种机制来改变以上各种情况的发生。

简单版客户端 socket 运行时可以接收任何地方发送过来的数据，
因此客户端 socket 也需要一种过滤机制，把那些不是服务端发送过来的消息过滤掉，
只处理指定服务器发送过来的数据。

因此 client 端代码需要改进以避免以上情况发生。


### client 端

1. 建立 UDP socket 对象
2. 将 socket 与指定地址 connect
3. 直接发送信息
4. 接收回信

注：
socket connect 时只是将指定地址存在 socket 对象中，并没有发送接受任何数据，
以便于此后的 send 函数默认发送地址和 recv 的默认接收地址。

示例如下：

```python
#!/usr/bin/env python

import socket

ADDR = ('127.0.0.1', 6666)
MAX = 65535
MESSAGE = 'hello world!'

s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.connect(ADDR)
delay = 0.1
data = None
while True:
    print('Send to server:', MESSAGE)
    s.send(MESSAGE.encode('utf-8'))
    s.settimeout(delay)
    try:
        data = s.recv(MAX)
    except socket.timeout:
        delay *= 2
        if delay > 2.0:
            raise RuntimeError('I think the server is shutdown')
    else:
        break

if data:
    print('The Server reply:', data.decode('utf-8'))
```

## socket 广播

注意广播地址主机标识段 host id 为全1。


## client 端

```python
#!/usr/bin/env python

import socket

ADDR = ('192.168.0.255', 6666)
MAX = 65535
MESSAGE = 'Broadcast message'

s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
s.sendto(MESSAGE.encode('utf-8'), ADDR)
```


## server 端

```python
#!/usr/bin/env python

import socket

ADDR = ('', 6666)
MAX = 65535

s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.bind(ADDR)
while True:
    data, addr = s.recvfrom(MAX)
    print('the client at', addr, 'say:', data.decode('utf-8'))
```
