created: 2018-04-18T20:48:13+08:00
tags: [kafka, python]


[上一篇][] 博文介绍了 kafka 集群的搭建，
本博文介绍如何用 [kafka-python][] 包来链接操作 kafka 集群。

[上一篇]: /posts/kafka/搭建kafka集群.html
[kafka-python]: https://github.com/dpkp/kafka-python


## 安装

```
pip install kafka-python
```

## 创建 kafka consumer

```
from kafka import KafkaConsumer

consumer = KafkaConsumer('test', bootstrap_servers='10.0.0.2:9092,10.0.0.3:9092,10.0.0.4:9092')
for msg in consumer:
    print (msg)
```

## 创建 kafka producer

```
from kafka import KafkaProducer

producer = KafkaProducer(bootstrap_servers='10.0.0.2:9092,10.0.0.3:9092,10.0.0.4:9092')
for _ in range(100):
    producer.send('test', b'some_message_bytes')
```


## 脚本测试

`producer.py` 文件如下：

```
import time
from kafka import KafkaProducer

producer = KafkaProducer(bootstrap_servers='10.0.0.2:9092,10.0.0.3:9092,10.0.0.4:9092')

i = 1
while True:
    message = f'message {i}'
    print(f'send message: {message}')
    producer.send('test', message.encode())
    i += 1
    time.sleep(3)
```

`consumer.py` 文件如下：

```
from kafka import KafkaConsumer, TopicPartition, OffsetAndMetadata


consumer = KafkaConsumer(bootstrap_servers='10.0.0.2:9092,10.0.0.3:9092,10.0.0.4:9092',
                         group_id='test_group',
                         auto_offset_reset="earliest",
                         enable_auto_commit=False)

tp = TopicPartition(topic='test', partition=0)
consumer.assign([tp])
print('start offset is ', consumer.position(tp))

for msg in consumer:
    print(msg)
    consumer.commit({tp: OffsetAndMetadata(msg.offset+1, "Some metadata")})
```

在两个终端上分别先后运行以下命令测试：

```
python consumer.py
```

和

```
python producer.py
```


## kafka reblance

当有多个 consumer 属于同一个消费者组时，kafka 会自动 reblance 的，
所以不需要在程序中显式的设置 topic partition 的，
所以 `consumer.py` 也可以这样写：

```
from kafka import KafkaConsumer, TopicPartition, OffsetAndMetadata


consumer = KafkaConsumer('test',
                         client_id='client1',
                         group_id='test_group',
                         bootstrap_servers='10.0.0.2:9092,10.0.0.3:9092,10.0.0.4:9092')

for msg in consumer:
    print(msg)
```

当运行多个 consumer 时，`client_id` 参数具有唯一性的，