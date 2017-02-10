date: 2017-02-10 16:09:15
tags: python, aiohttp

随着 CPython3.6 的发布，asyncio(异步 io 标准库)的 API 正式稳定下来了，
无疑利用 asyncio 写 web 应用将会极为方便。
虽然 python 现有已经存在一些异步 IO 网络框架(twisted, tornado, gevent)，
但它们都不能很好的利用 python3 的新特性(比如 async/await 关键字)，
所以决定尝试一下 [aiohttp][] 的 web 框架。

[aiohttp]: https://github.com/KeepSafe/aiohttp/


## 安装

```
pip install aiohttp
```


## hello world

虽然 aiohttp 经常用于客户端编程，但是它也可以用于服务端 web 开发，
而且写法也极为优美，下面是一个 hello world 示例：

```python
from aiohttp import web


async def hello(request):
    name = request.match_info.get('name', 'Anonymous')
    text = f'Hello, {name}'
    return web.Response(text=text)


app = web.Application()
app.router.add_get('/', hello)
app.router.add_get('/{name}', hello)

web.run_app(app)
```


## websocket

aiohttp 也原生支持 websocket：

```python
from aiohttp import web


async def ws_hello(request):
    ws = web.WebSocketResponse()
    await ws.prepare(request)

    async for msg in ws:
        if msg.type == web.MsgType.text:
            ws.send_str(f'Hello, {msg.data}')
        elif msg.type == web.MsgType.binary:
            ws.send_bytes(msg.data)
        elif msg.type == web.MsgType.close:
            break

    return ws


app = web.Application()
app.router.add_get('/echo', ws_hello)

web.run_app(app)
```


## 目录结构

虽然写一个简单的 web 服务一个文件就可以了，
但是当 web 服务比较复杂时，清晰的目录结构可以更有效的组织代码，
自己写一个服务时利用了以下代码结构：

```
backend
├── app/
│   ├── model/
│   ├── sql/
│   ├── api/
│   ├── config.py
│   ├── db.py
│   ├── middlewares.py
│   ├── permissions.py
│   ├── urls.py
│   └── utils.py
├── requirements/
│   ├── base.txt
│   ├── dev.txt
│   └── prod.txt
├── tests/
├── init／
├── deploy/
├── run.py
└── README.md
```

其中：

* `app` 目录存放所有服务相关的代码
* `app/model` 目录存放服务相关的 model 定义
* `app/sql` 目录存放数据库表格定义/修改的 sql 脚本
* `app/config.py` 存放所有相关配置信息
* `app/db.py` 存放与数据库操作与初始化相关的代码
* `app/middlewares.py` 存放 web 服务的中间件
* `app/permissions.py` 存放与权限检测相关的代码
* `app/urls.py` 存放 web 服务的路由定义
* `app/utils.py` 存放一些辅助工具函数
* `requirements` 目录存放 pip 安装 requirements 文件
* `init` 目录提供一些与数据库初始化和测试样例数据初始化的代码
* `deploy` 目录存放与服务部署相关的代码
* `run.py` 是服务的入口脚本，与 `flask-cli` 类似


## 相关包

asyncio 周边已经存在大量辅助包了，这里列一些比较常用的包：

* uvloop
* aiofiles
* asyncpg
* aioredis
* aiokafka
* aioes
