created: 2014-10-08T18:08:00+08:00
tags: [http, web]

这里记录了我在学习 Http:The Definitive Guide 时的一些笔记。


## MIME type

HTTP 可以传输任何格式的文件，
而这些格式的文件是由 MIME(Multipurpose Internet Mail Extensions)多用途互联网邮件扩展类型标识的。

常见的 MIME 类型有：

* text/plain: 普通文本，文件扩展名 `.txt`
* text/html：超文本标记语言文本，文件扩展名 `.html`
* text/xml: xml 文档，文件扩展名 `.xml`
* image/png: PNG 图像，文件扩展名 `.png`
* image/gif: GIF 图形，文件扩展名 `.gif`
* image/jpeg: JPEG 图形，文件扩展名 `.jpeg`,`.jpg`
* audio/x-wave: WAVE 格式的音频，文件扩展名 `.wav`
* audio/mpeg: MP3 格式的音频，文件扩展名 `.mp3`
* video/mpeg: MPEG 文件，文件扩展名 `.mpg`,`.mpeg`
* video/x-msvideo: AVI 文件，文件扩展名 `.avi`
* application/x-gzip: GZIP 文件，文件扩展名 `.gz`
* application/x-tar: TAR 文件，文件扩展名 `.tar`
* application/xhtml+xml: XHTML 文档，文件扩展名 `.xhtml`
* application/rtf: RTF 文本，文件扩展名 `.rtf`
* application/pdf: PDF 文档，文件扩展名 `.pdf`
* application/msword: Microsoft Word 文件，文件扩展名 `.word`
* application/octet-stream: 任意的二进制数据


## URI

URI(Uniform Resource Identifier)统一资源标识符，
有两种类型：

1. URL(Uniform Resource Locator)统一资源定位符，与位置有关，用位置定位

```
http://example.org/absolute/URI/with/absolute/path/to/resource.txt
```

2. URN(Uniform Resource Name)统一资源名，与位置无关，用名称定位

```
urn:issn:1535-3613
```

我们通常用 URL，极少用 URN。
URL 形式如下：

```
<scheme>://<user>:<password>@<host>:<port>/<path>;<parameters>?<query>#<frag>
```

1. scheme
1. user
1. password
1. host
1. port
1. path
1. parameters
1. query
1. frag

以上九部分并不需同时存在，例如：

```
http://www.example.com/index.html
http://www.example.com:80/index.html
http://8.8.8.8:80/index.html
http://www.example.com/index.html
ftp://ftp.example.com/pub/xxx
ftp://anonymous@ftp.example.com/pub/xxx
ftp://joe:joespasswd@ftp.example.com/pub/xxx
ftp://ftp.example.com/pub/xxx;type=
http://www.example.com/abc.html;sale=false/index.html;graphics=true
http://www.example.com/index.html?item=123&color=blue&size=large
http://www.example.com/index.html#id
```


## Message

Message 有三部分组成：

1. start line
1. header
1. body

两种 Message：

1. Request Message

```
<method> <request-URL> <version>
<headers>

<entity-body>
```

2. Response Message

```
<version> <status> <reason-phrase>
<headers>

<entity-body>
```

method:

* GET
* POST
* PUT
* DELETE
* HEAD
* TRACE
* OPTIONS

version:

```
HTTP/<major>.<minor>
```

header:

* general headers
* request headers
* response headers
* entity headers
* extension headers

response status codes:

* 100~199: informational
* 200~299: success
* 300~399: the resource has been moved
* 400~499: the client did something wrong in the request
* 500~599: something went awry on the server

经常用到的：

* 200: OK
* 301: Moved Permanently
* 302: Moved Temporarily
* 401: Unauthorized
* 404: NotFound
* 500: Internal Server Error
* 501: Not Implenmented


## 网路的组成

* Proxies
* Caches
* gateways
* tunnels
* agents
