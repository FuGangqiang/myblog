created: 2018-04-17T11:21:28+08:00
tags: [kafka, 分布式]

[kafka][] 是一个分布式的基于 push-subscribe 的消息系统，快速、可扩展、可持久化，
可以实时的处理大量数据以满足各种需求场景，具有以下特点：

- 高吞吐量、低延迟：每秒可以处理几十万条消息，它的延迟最低只有几毫秒
- 可扩展性：集群支持热扩展
- 持久性、可靠性：消息被持久化到本地磁盘，并且支持数据备份防止数据丢失
- 容错性：允许集群中节点失败（若副本数量为 n,则允许 n-1 个节点失败）
- 高并发：支持数千个客户端同时读写

[kafka]: https://kafka.apache.org

本博客讲解 debian9.4 系统下 kafka 集群的搭建。

## 集群节点配置

- `10.0.0.2`: zookeeper, kafka
- `10.0.0.3`: zookeeper, kafka
- `10.0.0.4`: zookeeper, kafka


## 安装 zookeeper

kafka 使用 zookeeper 来维护集群的状态，因此应该首先搭建一个 zookeeper 集群，
具体步骤请见博文：[搭建zookeeper集群][]。

[搭建zookeeper集群]: /posts/zookeeper/搭建zookeeper集群.html

## 安装 kafka

### 下载 kafka

```
wget http://mirrors.tuna.tsinghua.edu.cn/apache/kafka/1.1.0/kafka_2.12-1.1.0.tgz
```

### 安装 kafka

分别在三台节点机器上运行解压命令，解压到 `/opt` 目录：

```
tar -xzvf kafka_2.12-1.1.0.tgz -C /opt
```

## 创建 kafka 用户、组

```
groupadd kafka
useradd -g kafka -M -s /usr/sbin/nologin kafka
```

### 配置 kafka

创建 kafka 数据目录：

```
mkdir -p /var/lib/kafka
```

配置 `/opt/kafka_2.12-1.1.0/config/server.properties` 文件(`10.0.0.2` 节点)如下：

```
broker.id=2
listeners=PLAINTEXT://10.0.0.2:9092
num.network.threads=3
num.io.threads=8
socket.send.buffer.bytes=102400
socket.receive.buffer.bytes=102400
socket.request.max.bytes=104857600
log.dirs=/var/lib/kafka
num.partitions=1
num.recovery.threads.per.data.dir=1
offsets.topic.replication.factor=1
transaction.state.log.replication.factor=1
transaction.state.log.min.isr=1
log.retention.hours=168
log.segment.bytes=1073741824
log.retention.check.interval.ms=300000
zookeeper.connect=10.0.0.2:2181,10.0.0.3:2181,10.0.0.4:2181/kafka
zookeeper.connection.timeout.ms=6000
group.initial.rebalance.delay.ms=0
```

其中 `broker.id` 同 `zookeeper` 的 `myid` 类似，是集群中的唯一标识，在此设为 ip 地址尾数。

## 配置 systemd service

文件 `/etc/systemd/system/kafka.service` 文件如下：

```
[Unit]
Description=Kafka
After=zookeeper.service

[Service]
Type=simple
User=kafka
Group=kafka
ExecStart=/opt/kafka_2.12-1.1.0/bin/kafka-server-start.sh /opt/kafka_2.12-1.1.0/config/server.properties
Restart=always

[Install]
WantedBy=multi-user.target
```

在每一台机器上运行开启 kafka

```
systemctl start kafka
```

可以将 kafka 设为开机自启动：

```
systemctl enable kafka
```


## 测试 kafka 集群

### 创建 topic

```
/opt/kafka_2.12-1.1.0/bin/kafka-topics.sh \
    --zookeeper 10.0.0.2:2181,10.0.0.3:2181,10.0.0.4:2181/kafka \
    --create \
    --replication-factor 1 \
    --partitions 1 \
    --topic test
```

### 查看已经存在的 topic

```
/opt/kafka_2.12-1.1.0/bin/kafka-topics.sh \
    --zookeeper 10.0.0.2:2181,10.0.0.3:2181,10.0.0.4:2181/kafka \
    --list
```

### 接收消息

```
/opt/kafka_2.12-1.1.0/bin/kafka-console-consumer.sh \
    --bootstrap-server 10.0.0.2:9092,10.0.0.3:9092,10.0.0.4:9092 \
    --topic test \
    --from-beginning
```

### 发送消息

```
/opt/kafka_2.12-1.1.0/bin/kafka-console-producer.sh \
    --broker-list 10.0.0.2:9092,10.0.0.3:9092,10.0.0.4:9092 \
    --topic test
```
