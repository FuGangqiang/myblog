created: 2015-02-05T21:25:23+08:00
tags: [redis, 缓存, 数据库]

## 简介

redis(REmote DIctionary Server)是一个基于内存 key-value 的 NoSQL 存储引擎，
可以作为服务程序独立运行于自己的服务器主机上，
为了性能，redis 将数据全部放在内存里，
可以通过异步操作将数据写回磁盘实现数据的持久化。

不同于 memcached(只提供了 string 类型)，redis 支持多种数据类型，如 string(字符串)、list(列表)、set(集合)、zset(有序集合)、hash(散列)。


## Archlinux 安装

```
pacman -S redis
```


## 运行

运行服务端：

systemd 启动：
```
sudo systemctl start redis
```

默认配置文件 /etc/redis.conf，如果指定自己的配置文件：

```
redis-server redis.conf
```

终止服务端：

```
sudo systemctl stop redis
```

或

```
redis-cli shutdown
```


## 客户端

运行客户端：

```
redis-cli
```

默认本机端口6379，可以指定host和port：

```
redis-cli -h 127.0.0.1 -p 6379
```

和服务端交互：

```
127.0.0.1:637> ping
PONG
127.0.0.1:637> set foo bar
OK
127.0.0.1:637> get foo
"bar"
```


## redis命令


### key

只允许是字符串

测试指定key是否存在：

```
exits key
```
删除给定key：

```
del key1 key2 ... keyn
```

返回给定key的value类型：

```
type key
```

返回匹配指定模式的所有key，glob匹配：

```
keys pattern
```

返回从当前数据库中随机选择的一个key：

```
randomkey
```

重命名key：

```
rename oldkey newkey
```

只有newkey不存在时才会重命名：

```
renamenx oldkey newkey
```

指定过期相对时间：

```
expire key seconds
```

指定过期绝对时间：

```
expireat key timestamp
```

持久存储：

```
persist key
```

返回设置过过期时间的key的剩余过期秒数：

```
ttl key
```

返回排序后的原始列表：

```
sort key [by pattern] [limit offset count] [get pattern [get pattern ...]] [asc|desc] [alpha] [store destination]
```


### 数据库

redis是一个字典结构的存储服务器，一个字典就相当于一个独立的数据库，客户端可以指定将数据存储在哪个字典(数据库)里，
默认支持16个数据库，编号分别是0到15，可以通过配置参数databases修改数据库个数的上限。
客户端与redis建立连接后会自动选择0号数据库，但后续可以通过select命令自行切换，
redis的数据库更像是命名空间，用于在不同的数据库中存储同一个应用不同类型的数据。

通过索引选择数据库：

```
select db-index
```

返回当前数据库的key数量：

```
dbsize
```

将key从当前数据库移动到指定数据库：

```
move key db-index
```

删除当前数据库中所有key：

```
flushdb
```

删除所有数据库中的所有key：

```
flushall
```


### string类型

string类型是二进制安全的，也就是说，可以用string类型存储图片、音乐之类的。

设置key对应的值为value：

```
set key value
```

只有当key不存在时才设置：

```
setnx key value
```

原子操作，同时设置expire time：

```
setex key seconds value
```

获取key对应的value：

```
get key
```

原子操作，设置key的值为value，并返回key的旧value：

```
getset key value
```

一次获取多个key的value：

```
mget key1 key2 ... keyn
```

原子操作，设置多个key的value：

```
mset key1 value1 ... keyn valuen
```

原子操作，只有当key不存在时才设置：

```
msetnx key1 value1 ... keyn valueN
```

设置在指定offset上bit的值(0或1)：

```
setbit key offset value
```

返回在指定offset上bit的值：

```
getbit key offset
```

返回字符串长度：

```
strlen key
```

追加字符串：

```
append key value
```

替换部分字符串：

```
setrange key offset value
```

截取子字符串：

```
getrange key start end
```

加1操作：

```
incr key
```

减1操作：

```
decr key
```

加指定值：

```
incrby key integer
```

减指定值：

```
decrby key integer
```


### list类型

list类型就是每个元素都是string类型的双向链表。

在头部添加value：

```
lpush key value
```

只有当key存在时才设置：

```
lpushx key value
```

在尾部添加value：

```
rpush key value
```

只有当key存在时才设置：

```
rpushx key value
```

从头部删除元素：

```
lpop key
```

从尾部删除元素：

```
rpop key
```

原子操作，从source尾部删除一个元素，同时将其插入到destination头部：

```
rpoplpush source destination
```

与lpop相对应的阻塞命令：

```
blpop key1...keyn timeout
```

与rpop相对应的阻塞命令：

```
brpop key1 ... keyn timeout
```

与rpoplpush相对应的阻塞命令：

```
brpoplpush srckey destkey
```

返回key对应list的长度：

```
llen key
```

返回指定位置元素：

```
lindex key index
```

设置list中指定下标的元素值：

```
lset key index value
```

返回指定区间内的元素：

```
lrange key start end
```

截取list，保留指定区间内元素：

```
ltrim key start end
```

在pivot元素的前面或后面插入参数中的元素value：

```
linsert key before|after pivot value
```

从key对应list中删除count个和value相同的元素：

```
lrem key count value
```


### set类型

set类型就是每个元素都是string类型的无序集合。

添加元素到set：

```
sadd key value
```

移除元素：

```
srem key value
```

删除并返回set中随机的一个元素：

```
spop key
```

随机取set中的一个元素，但是不删除元素：

```
srandmember key
```

原子操作，从srckey对应set中移除value并添加到dstkey对应set中：

```
smove srckey dstkey member
```

返回set中元素个数：

```
scard key
```

判断value是否在set中：

```
sismember key value
```

返回所有给定key的交集：

```
sinter key1 key2 ... keyn
```

同时将交集存到dstkey：

```
sinterstore dstkey key1 ... keyn
```

返回所有给定key的并集：

```
sunion key1 key2...keyn
```

同时保存并集到dstkey下：

```
sunionstore dstkey key1 ... keyn
```

返回所有给定key的差集：

```
sdiff key1 key2...keyn
```

同时保存差集到dstkey下：

```
sdiffstore dstkey key1 ... keyn
```

返回key对应set的所有元素：

```
smembers key
```


### zset类型

zset类型就是每个元素都是string类型的有序集合。

添加元素到zset：

```
zadd key score value
```

删除元素：

```
zrem key value
```

增加对应value的score值：

```
zincrby key incr value
```

返回指定元素的排名：

```
zrank key member
```

按score逆序：

```
zrevrank key member
```

取指定区间的元素：

```
zrange key start end
```

按score逆序：

```
zrevrange key start end
```

返回score在给定区间的元素：

```
zrangebyscore key min max
```

逆序：

```
zrevrangebyscore key max min [withscores] [limit offset count]
```

返回score在给定区间的数量：

```
zcount key min max
```

返回元素个数：

```
zcard key
```

返回元素对应的score：

```
zscore key element
```

删除排名在给定区间的元素：

```
zremrangebyrank key min max
```

删除score在给定区间的元素：

```
zremrangebyscore key min max
```


### hash类型

hash类型就是一个string类型间的一个映射表。

配置字段最多64个：

```
hash-max-zipmap-entries 64
```

配置value最大为512字节：

```
hash-max-zipmap-value 512
```

映射field->value：

```
hset key field value
```

只有当key或field不存在时才设置：

```
hsetnx key field value
```

同时映射多个field->value：

```
hmset key filed1 value1 ... filedn valuen
```

获取field对应value：

```
hget key field
```

获取全部filed对应的value：

```
hmget key filed1 ... fieldn
```

返回指定hash的field数量：

```
hlen key
```

测试field是否存在：

```
hexists key field
```

删除field：

```
hdel key field
```

将指定filed 加上指定值：

```
hincrby key field integer
```

返回hash的所有field：

```
hkeys key
```

返回hash的所有value：

```
hvals key
```

获取该键包含的所有field/value：

```
hgetall key
```


### 事务

标记事务的开始：

```
multi
```

执行在一个事务内命令队列中的所有命令：

```
exec
```

回滚事务队列中的所有命令：

```
discard
```

指定待监控的keys：

```
watch key1 key2 ... keyn
```

取消当前事务中指定监控的keys：

```
unwatch
```


### 系统

密码验证：

```
auth passwd
```

退出服务器：

```
shutdown [nosave] [save]
```

打印字符串：

```
echo message
```

查看服务是否运行：

```
ping
```

关闭当前连接：

```
quit
```

查看服务器状态信息：

```
info
```

返回当前服务器时间：

```
time
```

返回最近一次redis成功将数据保存到磁盘上的时间，以unix时间戳格式表示：

```
lastsave
```

获取连接的名称：

```
client list
```

关闭客户端连接：

```
client kill [ip:port] [id client-id]
```

获取连接到服务器的客户端连接列表：

```
client getname
```

设置当前连接的名称：

```
client setname connection-name
```

在指定时间内终止运行来自客户端的命令(单位是毫秒)：

```
client pause timeout
```

获取集群节点的映射数组：

```
cluster slots
```

实时打印出redis服务器接收到的命令，调试用：

```
monitor
```

获取redis命令详情数组：

```
command
```

获取redis命令总数：

```
command count
```

获取给定命令的所有键：

```
command getkeys
```

获取指定redis命令描述的数组：

```
command info command-name [command-name ...]
```

获取key的调试信息：

```
debug object key
```

让redis服务崩溃：

```
debug segfault
```

读取执行时间较长的命令：

```
slowlog subcommand [argument]
```

查看某个指定的服务器配置值：

```
config get parameter
```

在不重启服务器的情况下动态修改配置：

```
config set parameter value
```

对启动redis服务器时所指定的redis.conf配置文件进行改写：

```
config rewrite
```

reset info命令给出的统计数字：

```
config resetstat
```

修改slave服务器的复制设置：

```
slaveof host port
```

设置RDB持久化模式的保存策略：

```
save
```

异步执行一个AOF(AppendOnly File)文件重写操作：

```
bgrewriteaof
```

后台异步保存当前数据库的数据到磁盘：

```
bgsave
```

返回主从实例所属的角色：

```
role
```

用于复制功能(replication)的内部命令：

```
sync
```


## python客户端演示

许多编程语言都有redis的客户端，python客户端安装：

```
pip install redis
```

运行示例：

```
import redis

r = redis.StrictRedis(host='localhost', port=6379, db=0)
r.set('foo', 'bar')
value = r.get('foo')
```
