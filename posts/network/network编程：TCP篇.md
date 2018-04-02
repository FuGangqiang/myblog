created: 2015-03-30T17:26:00+08:00
tags: [python, network]


TCP(Transmission Control Protocol，传输控制协议)是一种面向连接的、可靠的、基于字节流的通信协议，
它和 UDP 一样，都处于 TCP/IP 协议的传输层，但是比 UDP 复杂许多。

为了使数据在传输过程中不会发生丢包、变化、错序或出现网络拥堵现象，TCP 自身实现了一系列的功能：

* 请求应答方式建立或中止连接
* 校验接收数据
* 滑动窗口协议
* 超时重传、丢弃重复数据
* 流量控制

正是由于 TCP 的可靠性，互联网上的许多协议都是基于 TCP 之上的，比如 FTP、Telnet、SMTP、HTTP、POP3 等。

但是由于 TCP 是面向连接的，所以其不能像 UDP 那样实现广播的功能。


## socket 简单版


### client 端

* 建立 TCP socket 对象
* connect 指定地址服务器
* 发送数据
* 接受数据

示例如下：

```python
#!/usr/bin/env python

import socket

ADDR = ('127.0.0.1', 6666)
MESSAGE = 'hello world!'

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(ADDR)

print('Send to server:', MESSAGE)
s.sendall(MESSAGE.encode('utf-8'))
data = s.recv(1024)
print("the server says:", data.decode('utf-8'))

s.close()
```


### server 端

* 建立 TCP socket 对象
* 绑定端口地址
* 监听并生成连接
* 接受数据
* 发送回复

示例如下：

```python
#!/usr/bin/env python

import socket

ADDR = ('127.0.0.1', 6666)

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.bind(ADDR)
s.listen(1)

while True:
    sock, addr = s.accept()
    print('We have accepted a connection from', addr)
    message = sock.recv(1024)
    print('the client said', message.decode('utf-8'))
    reply = 'you messagge is {} bytes'.format(len(message))
    sock.sendall(reply.encode('utf-8'))
    sock.close()
```

## socket 改进版

由于 TCP 的一系列特性，它与 UDP 代码表面看起来差不多，但背后却大不相同：

* connect 时将会与服务端进程发生三次握手协议来建立真正连接，这也就意味着如果服务端禁止连接，connect 可能会失败发生异常。
* 服务端可以同时连接许多客户端，这就要求服务端有一个监听 socket 和与每个客户端相对应的活动 socket，
  监听 socket 只是负责监听端口和建立连接，而活动 socket 才是用来和客户端通信的。
* 当服务端进行频繁重启时，bind 可能由于端口地址正在被使用而发生失败，因为操作系统会将上一次 TCP 连接维持一段时间以彻底终止上次连接。
* 在 UDP 中每一次 send 和 recv 都是意味着发送或接收一个数据包，但是在 TCP 中完全不同，每一个 TCP 连接都会有数据缓冲区，
  当缓冲区满后才会 send 数据，或当缓冲区有数据时才能 recv 到数据（这里有点不严谨，但已说明问题），
  所以 TCP 和 UDP 在传输数据时，send 和 recv 完全不同，也就是说每一次 send 时，有可能不会把数据全部发送完，
  每一次 recv 时，有可能不会接受到指定字节数的数据。
* 每一次连接结束后，都须关闭连接。

TCP 由于以上特点，我们的代码需要改进许多。


### client端

```python
#!/usr/bin/env python

import socket

def recv_all(sock, count):
    buf = b''
    while len(buf) < count:
        more = sock.recv(count-len(buf))
        if not more:
            raise EOFError('socket closed {:d} bytes into a {:d}-byte message'
                           .format(len(buf), count))
        buf += more
    return buf

ADDR = ('127.0.0.1', 6666)
MESSAGE = 'hello world!'

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(ADDR)

print('Send to server:', MESSAGE)
message_bytes = MESSAGE.encode('utf-8')
message_length_bytes = '{:04d}'.format(len(message_bytes)).encode('utf-8')
s.sendall(message_length_bytes)
s.sendall(message_bytes)
reply_length_buf = recv_all(s, 4)
reply_length = int(reply_length_buf.decode('utf-8'))
reply = recv_all(s, reply_length)
print("the server says:", reply.decode('utf-8'))

s.close()
```


### server端

```python
#!/usr/bin/env python

import socket

def recv_all(sock, count):
    buf = b''
    while len(buf) < count:
        more = sock.recv(count-len(buf))
        if not more:
            raise EOFError('socket closed {:d} bytes into a {:d}-byte message'
                           .format(len(buf), count))
        buf += more
    return buf

ADDR = ('127.0.0.1', 6666)

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind(ADDR)
s.listen(1)

while True:
    sock, addr = s.accept()
    print('We have accepted a connection from', addr)
    message_length_buf = recv_all(sock, 4)
    message_length = int(message_length_buf.decode('utf-8'))
    message = recv_all(sock, message_length)
    print('the client said', message.decode('utf-8'))
    reply_buf = 'you messagge is {} bytes'.format(message_length).encode('utf-8')
    reply_length_buf = '{:04d}'.format(len(reply_buf)).encode('utf-8')
    sock.sendall(reply_length_buf)
    sock.sendall(reply_buf)
    sock.close()
```
