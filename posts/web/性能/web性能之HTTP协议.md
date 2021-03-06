created: 2016-10-01T17:37:55+08:00
tags: [web, 性能]


## HTTP 协议历史


#### HTTP 0.9

1991 年，Tim Berners-Lee 以简洁为出发点设计最初的 HTTP 协议，此时版本号被定为 0.9。

请求只有一行，包括 GET 方法和要请求的文档的路径。
响应是一个超文本文档，没有首部，也没有其他元数据，只有 HTML。
这实在是简单得不能再简单了！


#### HTTP 1.0

随着人们对新兴 Web 的需求越来越多，以及它们在公共 Web 上的应用迅速爆发，HTTP 0.9 的很多根本性不足便暴露出来。
人们需要一种协议，它不仅能访问超文本文档，还能提供有关请求和响应的各种元数据，而且要支持内容协商，等等。
相应地，新兴的 Web 开发者社区为满足这些需求，推出了大量实验性的 HTTP 服务器和客户端实现，基本上遵循实现、部署、推广采用的流程。

就在这些实验性开发的基础上，出现了一套最佳实践和共用模式。
于是，1996 年，HTTP 工作组发布了 RFC 1945，解释说明了当时很多 HTTP 1.0 实现的“公共用法”。
不过，这个 RFC 只是参考性的。HTTP 1.0 并不是一个正式的规范或互联网标准！

请求和响应首部都使用 ASCII 编码，但响应对象本身可以是任何类型：HTML 文件、纯文本文件、图片，或其他内容类型。
这样，HTTP 迅速发展为超媒体传输协议。

除了媒体类型协商，RFC 还解释了很多已经被实现的其他功能：内容编码、字符集支持、多部分类型、认证、缓存、代理行为、日期格式，等等。


#### HTTP 1.1

就在 HTTP 1.0 发布大约 6 个月之后，也就是 1997 年 1 月，正式的 HTTP 1.1 标准的 RFC 2068 也发布了。
又过了两年半，即到了 1999 年 6 月 RFC 2616 发布，又在标准中集合了很多改进和更新。

HTTP 1.1 标准厘清了之前版本中很多有歧义的地方，而且还加入了很多重要的性能优化：
持久连接、分块编码传输、字节范围请求、增强的缓存机制、传输编码及请求管道。

此外，HTTP 1.1 协议添加了内容、编码、字符集，甚至语言的协商机制，
还添加了传输编码、缓存指令、客户端 cookie 等十几个可以每次请求都协商的字段。


#### HTTP 2.0

HTTP 的简单本质是它最初得以采用和后来快速发展的关键，
然而，今天，用户和 Web 开发者都迫切想要通过 HTTP 1.1 达到一种几近实时的响应速度和协议性能，
而要满足这个需求，仅靠在原协议基础上修修补补是不够的，因此，HTTP 工作组于 2012 年宣布要开发 HTTP 2.0，
当然，它的工作最初是基于 Google 2009 年中发布的 SPDY 实验性协议。

经过 2012 年到 2014 年间的草案编辑和试验，最终于 2015 年 5 月发布了 HTTP 2.0 标准 RFC 7540。

HTTP 2.0 的目的就是通过支持请求与响应的多路复用来减少延迟，
通过压缩 HTTP 首部字段将协议开销降至最低，
同时增加对请求优先级和服务器端推送的支持。
为达成这些目标，HTTP 2.0 还给我们带来大量其他协议层面的辅助实现，比如新的流量控制、错误处理和更新机制。


## 优化 HTTP

当然，升级现有服务器和客户端以充分利用 HTTP 的最新特性以及性能优化是最简单的方法，
但是，现有的基础设施大多还是针对 HTTP 1.1 的，全部升级到 HTTP 2.0 并不是一时就可以完成的，
所以优化现有 HTTP 还需针对 HTTP 的版本。

但是优化的总体原则是不变的：

* 减少 DNS 查找
* 重用 TCP 连接
* 减少 HTTP 重定向
* 使用 CDN（内容分发网络）
* 去掉不必要的资源
* 在客户端缓存资源
* 传输压缩过的内容
* 消除不必要的请求开销
* 并行处理请求和响应
* 针对协议版本采取优化措施


#### HTTP 1.1 优化

* 利用 HTTP 管道
* 采用域名分区
* 打包资源以减少 HTTP 请求
* 嵌入小资源

但是这些优化手段各有利弊，如果某些优化措施过于激进或使用不当，反倒会伤害性能。


#### HTTP 2.0 优化

* 服务器的初始 cwnd 应该是 10 个分组
* 服务器应该通过 ALPN（针对 SPDY 则为 NPN）协商支持 TLS
* 服务器应该支持 TLS 恢复以最小化握手延迟
* 每个来源使用一个连接
* 去掉不必要的文件合并和图片拼接
* 利用服务器推送（比如嵌入资源）
* 不要采用采用域名分区


注：本文为《web 性能权威指南》笔记
