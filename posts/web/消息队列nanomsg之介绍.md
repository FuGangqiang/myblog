date: 2017-01-10 12:09:00
tags: mq, nanomsg, web


在众多消息队列中，最简单、灵活、快速的应该就是 `nanomsg` 吧，
nanomsg 是 zeromq 作者重新用 C 语言重新实现的，
是对 zeromq 的经验教训的各种提炼和反思。

> We add power by removing complexity rather than exposing new functionality.
>
> -- zeromq doc

nanomq 提供了 6 种通信模式，也即所谓“扩展性协议”：

* pipeline
* reqrep
* pair
* bus
* pubsub
* survey

这些扩展性协议是建立在传输层之上的，现在 nanomsg 提供了 3 种传输机制：

* inproc(单进程内通信)
* ipc(单机内多进程通信)
* tcp(通过 tcp 协议的网络通信)

nanomsg 的所有操作都是基于不同类型的 Socket，而 Socket 的类型决定了 nanomsg 使用了哪种通信模式和传输机制。

以下示例用 python 语言编写。


## 安装 nanomsg

```
sudo pacman -S nanomsg
```


## Pipeline(A One-Way Pipe)

![pipeline](/media/nanomsg/pipeline.png)

文件 `pipeline.py` 如下：

```
import nnpy


def producer(url, msg):
    sock = nnpy.Socket(nnpy.AF_SP, nnpy.PUSH)
    sock.bind(url)
    print(f'[Producer]: send "{msg}"')
    sock.send(msg)


def consumer(url):
    sock = nnpy.Socket(nnpy.AF_SP, nnpy.PULL)
    sock.connect(url)
    while True:
        msg = sock.recv()
        print(f'[Consumer]: receive "{msg}"')


if __name__ == '__main__':
    import sys
    assert len(sys.argv) >= 2
    if sys.argv[1] == 'producer':
        assert len(sys.argv) == 4
        producer(sys.argv[2], sys.argv[3])
    elif sys.argv[1] == 'consumer':
        assert len(sys.argv) == 3
        consumer(sys.argv[2])
    else:
        print('''Usage:
        python pipeline.py producer <url> <msg>
        python pipeline.py consumer <url>''')
```

终端测试：

```
# 终端１
python pipeline.py consumer 'ipc:///tmp/pipeline.ipc'
# 终端2
python pipeline.py producer 'ipc:///tmp/pipeline.ipc' 'hello world'
```


## Request/Reply (I ask, you answer)

![reqrep](/media/nanomsg/reqrep.png)

文件 `reqrep.py` 如下：

```
import nnpy


def server(url):
    sock = nnpy.Socket(nnpy.AF_SP, nnpy.REP)
    sock.bind(url)
    while True:
        msg = sock.recv()
        print(f'[Server]: received "{msg}"')
        sock.send(msg)
        print(f'[Server]: sent "{msg}"')


def client(url, msg):
    sock = nnpy.Socket(nnpy.AF_SP, nnpy.REQ)
    sock.connect(url)
    print(f'[Client]: send "{msg}"')
    sock.send(msg)
    msg = sock.recv()
    print(f'[Client]: received "{msg}"')


if __name__ == '__main__':
    import sys
    assert len(sys.argv) >= 2
    if sys.argv[1] == 'server':
        assert len(sys.argv) == 3
        server(sys.argv[2])
    elif sys.argv[1] == 'client':
        assert len(sys.argv) == 4
        client(sys.argv[2], sys.argv[3])
    else:
        print('''Usage:
        python reqrep.py server <url>
        python reqrep.py client <url> <msg>''')
```

终端测试：

```
# 终端１
python reqrep.py server 'ipc:///tmp/reqrep.ipc'
# 终端2
python reqrep.py client 'ipc:///tmp/reqrep.ipc' 'hello world'
```


## Pair (Two Way Radio)

![pair](/media/nanomsg/pair.png)


文件 `pair.py` 如下：

```
import time
import nnpy


def node1(url):
    sock = nnpy.Socket(nnpy.AF_SP, nnpy.PAIR)
    sock.bind(url)
    name = 'Node1'
    while True:
        msg = sock.recv()
        print(f'[{name}] received {msg}')
        time.sleep(1)
        print(f'[{name}] send {name}')
        sock.send(name)


def node2(url):
    sock = nnpy.Socket(nnpy.AF_SP, nnpy.PAIR)
    sock.connect(url)
    name = 'Node2'
    while True:
        print(f'[{name}] send {name}')
        sock.send(name)
        time.sleep(1)
        msg = sock.recv()
        print(f'[{name}] received {msg}')


if __name__ == '__main__':
    import sys
    assert len(sys.argv) == 3
    if sys.argv[1] == 'node1':
        node1(sys.argv[2])
    elif sys.argv[1] == 'node2':
        node2(sys.argv[2])
    else:
        print('''Usage:
        python pair.py node1 <url>
        python pair.py node1 <url>''')
```

终端测试：

```
# 终端１
python pair.py node1 'ipc:///tmp/pair.ipc'
# 终端2
python pair.py node2 'ipc:///tmp/pair.ipc' 'hello world'
```


## Pub/Sub (Topics & Broadcast)

![pubsub](/media/nanomsg/pubsub.png)


文件 `pubsub.py` 如下：

```
import time
import nnpy


def publisher(url):
    sock = nnpy.Socket(nnpy.AF_SP, nnpy.PUB)
    sock.bind(url)
    while True:
        msg = str(time.time())
        print(f'[Publisher] publish {msg}')
        sock.send(msg)
        time.sleep(1)


def subscriber(url, name):
    sock = nnpy.Socket(nnpy.AF_SP, nnpy.SUB)
    sock.setsockopt(nnpy.SUB, nnpy.SUB_SUBSCRIBE, "")
    sock.connect(url)

    while True:
        msg = sock.recv()
        print(f'[{name}] received {msg}')


if __name__ == '__main__':
    import sys
    assert len(sys.argv) == 3
    if sys.argv[1] == 'publisher':
        publisher(sys.argv[2])
    elif sys.argv[1].startswith('subscriber'):
        subscriber(sys.argv[2], sys.argv[1])
    else:
        print('''Usage:
        python pubsub.py publisher <url>
        python pubsub.py subscriber<n> <url>''')
```

终端测试：

```
# 终端１
python pubsub.py publisher 'ipc:///tmp/pubsub.ipc'
# 终端2
python pubsub.py subscriber1 'ipc:///tmp/pubsub.ipc'
# 终端3
python pubsub.py subscriber2 'ipc:///tmp/pubsub.ipc'
# 终端4
python pubsub.py subscriber3 'ipc:///tmp/pubsub.ipc'
```


## Survey (Everybody Votes)

![survey](/media/nanomsg/survey.png)

文件 `survey.py` 如下：

```
import time
import nnpy


def surveyor(url):
    sock = nnpy.Socket(nnpy.AF_SP, nnpy.SURVEYOR)
    sock.bind(url)
    time.sleep(1)
    msg = str(time.time())
    print(f'[Surveyor] request {msg}')
    sock.send(msg)
    while True:
        try:
            msg = sock.recv()
            print(f'[Surveyor] receive {msg}')
        except nnpy.errors.NNError:
            break


def respondent(url, name):
    sock = nnpy.Socket(nnpy.AF_SP, nnpy.RESPONDENT)
    sock.connect(url)

    while True:
        msg = sock.recv()
        print(f'[{name}] received {msg}')
        print(f'[{name}] response {msg}')
        sock.send(msg)


if __name__ == '__main__':
    import sys
    assert len(sys.argv) == 3
    if sys.argv[1] == 'surveyor':
        surveyor(sys.argv[2])
    elif sys.argv[1].startswith('respondent'):
        respondent(sys.argv[2], sys.argv[1])
    else:
        print('''Usage:
        python survey.py surveyor <url>
        python survey.py respondent<n> <url>''')
```


终端测试：

```
# 终端１
python survey.py respondent1 'ipc:///tmp/survey.ipc'
# 终端2
python survey.py respondent2 'ipc:///tmp/survey.ipc'
# 终端3
python survey.py respondent3 'ipc:///tmp/survey.ipc'
# 终端4
python survey.py surveyor 'ipc:///tmp/survey.ipc'
```


## Bus (Routing)

![bus](/media/nanomsg/bus.png)

文件 `bus.py` 如下：

```
import time
import nnpy


def node(url, name, *other_urls):
    sock = nnpy.Socket(nnpy.AF_SP, nnpy.BUS)
    sock.bind(url)
    for url in other_urls:
        sock.connect(url)
    time.sleep(5)
    sock.setsockopt(nnpy.SOL_SOCKET, nnpy.RCVTIMEO, 5000)
    print(f'[{name}] send {name} onto bus')
    sock.send(name)
    while True:
        try:
            msg = sock.recv()
            print(f'[{name}] receive {msg} from bus')
        except nnpy.errors.NNError:
            break


if __name__ == '__main__':
    import sys
    assert len(sys.argv) >= 4
    if sys.argv[1].startswith('node'):
        node(sys.argv[2], sys.argv[1], *sys.argv[3:])
    else:
        print('''Usage:
        python bus.py node<n> <url> <url>...''')
```


终端测试：

```
# 终端１
python bus.py node1 'ipc:///tmp/node1.ipc' \
    'ipc:///tmp/node2.ipc' \
    'ipc:///tmp/node3.ipc' \
    'ipc:///tmp/node4.ipc'
# 终端2
python bus.py node2 'ipc:///tmp/node2.ipc' \
    'ipc:///tmp/node1.ipc' \
	'ipc:///tmp/node3.ipc' \
	'ipc:///tmp/node4.ipc'
# 终端3
python bus.py node3 'ipc:///tmp/node3.ipc' \
    'ipc:///tmp/node1.ipc' \
	'ipc:///tmp/node2.ipc' \
	'ipc:///tmp/node4.ipc'
# 终端4
python bus.py node4 'ipc:///tmp/node4.ipc' \
    'ipc:///tmp/node1.ipc' \
	'ipc:///tmp/node2.ipc' \
	'ipc:///tmp/node3.ipc'
```
