created: 2015-02-04T22:17:24+08:00
tags: [memcached, 缓存, 数据库]

许多 Web 应用都将数据保存到 RDBMS 中，应用服务器从中读取数据并在浏览器中显示。
但随着数据量的增大、访问的集中，就会出现 RDBMS 的负担加重、数据库响应恶化、网站显示延迟等重大影响。
这时的解决办法之一就是利用内存来缓存数据库的查询结果，此后再次访问相同的数据，直接从内存读取，
而不再查询数据库，进而减轻数据库的负担。

memcached 就是为解决此种问题而产生的。


## 简介

memcached 是高性能的分布式内存缓存服务器，
通过缓存数据库查询结果，减少数据库访问次数，以提高动态应用的速度、提高可扩展性。

为了提高性能，memcached 将数据都存储在内存存储空间中，基于一个存储键/值对的 hashmap。
由于数据仅存在于内存中，因此重启 memcached、重启操作系统都会导致全部数据消失。
另外，内容容量达到指定值之后，就基于 LRU(Least Recently Used)算法自动删除不使用的缓存。
memcached 本身是为缓存而设计的服务器，因此并没有过多考虑数据的永久性问题。

memcached 作为高速运行的分布式缓存服务器，具有以下的特点：

* 协议简单
* 基于 libevent 的事件处理
* 内置内存存储方式
* memcached 不互相通信的分布式


## Archlinxu 安装

```
pacman -S memcached
```


## 运行

```
memcached -d -p 1234 -m 1024
```

其中：

* `-d`：以守护进程运行 memcached
* `-p`：指定 memcached 服务器监听端口号默认为 11211
* `-m`：设置 memcached 可以使用内存的大小，单位为 M


## 客户端

memcached 使用简单的文本行协议，
因此可以用 telnet 与 memcached 服务器进行交互，
例如访问本机上监听 1234 端口的 memcached 服务器：

```
telnet localhost 1234
```

这样就可以键入 memcached 命令与之交互了。

memcached 服务端是用 ANSI C 写成的，
但客户端可以用任何语言利用 socket 来与之通信。

例如：python3 中的 python3-memcached 模块就是利用其 socket 模块写成的。


## python客户端安装

```
pip install python3-memcached
```


## 命令

memcached 协议提供的命令很简单：

* set：无论如何都进行存储

```
set key flags exptime bytes [noreply]
value
```

* add：只有数据不存在时才进行添加

```
add key flags exptime bytes [noreply]
value
```

* replace：只有数据存在时才进行替换

```
replace key flags exptime bytes [noreply]
value
```

* append：当数据存在时，添加数据至其首

```
append key flags exptime bytes [noreply]
value
```

* prepend：当数据存在时，添加数据至其尾

```prepend key flags exptime bytes [noreply]
value
```

* cas：check and set之意，只有unique_cas_token与gets所获取的参数匹配才能存储

```
cas key flags exptime bytes unique_cas_token [noreply]
value
```

* get：读取

```
get key1 [key2 key3]
```

* gets：读取

```
gets key1 [key2 key3]
```

* delete：删除

```
delete key [noreply]
```

* incr：当值为数字时，增加其值

```
incr key increment_value
```

* decr：当值为数字时，减少其值

```
decr key decrement_value
```

* stats：统计信息

* flush_all：清除

```
flush_all [time] [noreply]
```

## python 客户端示例

摘自python3-memcached源码：

```python
import memcache
mc = memcache.Client(['127.0.0.1:11211'], debug=0)

mc.set("some_key", "Some value")
value = mc.get("some_key")

mc.set("another_key", 3)
mc.delete("another_key")

mc.set("key", "1")   # note that the key used for incr/decr must be a string.
mc.incr("key")
mc.decr("key")
```

一般用下面的方法来缓存数据库：

```python
key = derive_key(obj)
obj = mc.get(key)
if not obj:
    obj = backend_api.get(...)
    mc.set(key, obj)

# we now have obj, and future passes through this code
# will use the object from the cache.
```
