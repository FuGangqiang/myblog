created: 2018-04-16T15:40:18+08:00
tags: [zookeeper, python]

[上一篇][] 博文介绍了 zookeeper 集群的搭建，
本博文介绍如何用 [python kazoo][] 包来链接操作 zookeeper 集群。

[上一篇]: /posts/zookeeper/搭建zookeeper集群.html
[python kazoo]: https://github.com/python-zk/kazoo

## 安装

```
pip install kazoo
```

## 创建 zookeeper client

```
from kazoo.client import KazooClient

zk = KazooClient(hosts='10.0.0.2:2181')

zk.start()
# some operations
zk.stop()
```

上面的 `zk` 对象就是 zookeeper client 的一个实例，
所有针对 zookeeper 的操作都是通过 zk 对象操作的。

## zookeeper 命令

`zk` 对象以下方法与 zookeeper cli client 命令类似：

* ensure_path
* create
* get
* get_children
* set
* delete

```
zk.ensure_path("/my/favorite")
zk.create("/my/favorite/node", b"a value")

if zk.exists("/my/favorite"):
    data, stat = zk.get("/my/favorite")
    print(f'Version: {stat}, data: {data}')
    children = zk.get_children("/my/favorite")
    print(f'Children are now: {children}')

zk.set("/my/favorite", b"some data")
zk.delete("/my", recursive=True)
```

kazoo 也提供了 `ChildrenWatch` 和 `DataWatch` 的装饰器来监控 zookeeper 里面的数据。


## 脚本测试

`watch.py` 文件如下：

```
from kazoo.client import KazooClient


class ZooKeeper(KazooClient):
    def __init__(self, timeout=15, *args, **kwargs):
        super().__init__(timeout=timeout, *args, **kwargs)
        self.start(timeout)

    def __del__(self):
        self.stop()


zk = ZooKeeper(hosts='10.0.0.2:2181,10.0.0.3:2181,10.0.0.4:2181')
zk.ensure_path('/app/hello')

@zk.ChildrenWatch('/app/hello')
def watch_children(children):
    print(f'Children are now: {children}')


@zk.DataWatch('/app/hello/database')
def watch_node(data, stat):
    print(f'Version: {stat}, data: {data}')


input('press any key to stop:')
```

`modify.py` 文件如下：

```
from kazoo.client import KazooClient


class ZooKeeper(KazooClient):
    def __init__(self, timeout=15, *args, **kwargs):
        super().__init__(timeout=timeout, *args, **kwargs)
        self.start(timeout)

    def __del__(self):
        self.stop()


zk = ZooKeeper(hosts='10.0.0.2:2181,10.0.0.3:2181,10.0.0.4:2181')
zk.ensure_path('/app/hello')

zk.create('/app/hello/database', b'10.0.0.6:5432')
# zk.set('/app/hello/database', b'10.0.0.6:3306')
```

在两个终端上分别先后运行以下命令测试：

```
python watch.py
```

和

```
python modify.py
```