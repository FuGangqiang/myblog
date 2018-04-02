created: 2017-01-11T11:13:42+08:00
tags: [web, rabbitmq, 消息队列]


随着软件系统规模的不断增长、扩展，它会变得越来越复杂，
为了便于管理，大型的软件系统一定会被解藕，分为很多的子组件、子系统或模块，
各个模块间需要相互通信，消息队列服务便是为此而生。
本文讲解的是 RabbitMQ。

RabbitMQ 是一个由 erlang 语言开发的 AMQP（Advanced Message Queue）的开源实现。


## 安装

```
sudo pacman -S rabbitmq
```

配置文件为 `/etc/rabbitmq/rabbitmq-env.conf`。


## 启动 rabbitmq 服务

```
sudo systemctl start rabbitmq
```

## Hello World 示例

![hello world](/media/rabbitmq/hello_world.png)


以下代码用 `python` 语言编写。


### 生产者

```python
import pika

connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()
channel.queue_declare(queue='hello')

body = 'Hello World!'
channel.basic_publish(exchange='', routing_key='hello', body=body)
print(f'[x] Sent {body}')

connection.close()
```


### 消费者

```python
import pika

connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()
channel.queue_declare(queue='hello')

def callback(ch, method, properties, body):
    print(f'[x] Received {body}')

channel.basic_consume(callback, queue='hello', no_ack=True)
channel.start_consuming()
```
