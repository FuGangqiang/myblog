created: 2014-05-29T21:52:08+08:00
tags: [linux, CLI]


curl(Client for URLs，发音为 'see URL')，一个网络命令行工具，
支持许多协议（DICT，FILE，FTP，FTPS，GOPHER，HTTP，HTTPS，IMAP，IMAPS，LDAP，LDAPS，
POP3，POP3S，RTMP，RTSP，SCP，SFTP，SMB，SMBS，SMTP，SMTPS，TELNET 和  TFTP），
本篇只针对 HTTP 协议。

curl 可以制作 http 请求，获取 http 响应。
一个 http 请求由 request method(GET、POST、HEAD、PUT、DELETE等)、request headers 和 request body 组成，
一个 http 响应由 status line、response headers 和 response body 组成。


## example

获取一个网页：

```
curl http://example.com/
```


也可以给网址添加 query string：

```
curl www.baidu.com/s?wd=abc
```


同时获取两个网页：

```
curl http://example1.com/ http://example2.com/
```


如果你进行 web 调试或者想看一下 curl 和 服务器之间的交互情况，通常可以利用 `-v` 或 `--verbose` 参数：

```
curl -v http://example.com/
```


有时 `-v` 还不能满足你的要求，你可以用 `--trace` 或 `--trace-ascii` 参数：

```
curl --trace output.txt http://example.com/
```


如果你还想查看 http 在传输过程中所用的时间可以添加 `--trace-time` 参数：

```
curl --trace-ascii d.txt --trace-time http://example.com/
```


curl 命令通常将结果输出到标准输出，如果想要结果重定向某文件，可以使用 `-o` 或 `--output` 参数：

```
curl -o filename http://example.com/
```


也可以用 `-O` 或 `--remote-name` 参数使输出文件名与远程文件一样：

```
curl -O filename http://example.com/
```


仅获取 headers，可以利用 `-I` 或 `--head` 参数：

```
curl -I www.baidu.com
```


自动进行重定向可以利用 `-L` 或 `--location` 参数：

```
curl -L http://redirect.test.com
```


指定 http 请求方法：`GET`、`POST`、`PUT`、`DELETE`等

```
curl -X POST http://example.com/
```


添加或删除 http 请求某头信息可以利用 `-H` 或 `--header` 参数：

```
curl -H "Accept:" http://example.com/
curl -H "Accept: application/json" -H "Authorization: OAuth 2c3455d1aeffc" http://example.com/
```


利用参数 `-d` 或 `--data` 发送 POST 请求包体：

```
curl -d "firstname=abc" -d "lastname=def" http://example.com/
```

上面发送的消息头里还有 `Content-Type: application/x-www-form-urlencoded`，
而 request body 为 `firstname=abc&lastname=def`。

当然，你也可以想下面这样：

```
curl -d "firstname=abc&lastname=def" http://example.com/
```

`-d` 参数发送的数据必须是已经转义后的格式，比如空格是 `%20`，如果要 curl 帮你转义，可以利用 `--data-urlencode` 参数：

```
curl --data "birthyear=1905&press=%20OK%20"  http://example.com
curl --data-urlencode "birthyear=1905&press= OK "  http://example.com
```

当参数 `-d` 或 `--data` 去 POST 多个 url 时，POST 会发送相同的数据到那些 url：

```
curl --data name=curl http://example1.com/ http://example2.com/
```


如果要避免同时发送数据，可以利用 `-:` 或 `--next` 参数，下面是先发送一个 POST 请求，然后在发送一个 GET 请求：

```
curl -d name=curl http://example1.com/ --next http://example2.com/
```

POST 时，当 `enctype='multipart/form-data'` 时，可以利用 `-F` 或 `--form` 参数：

```
curl -F upload=@filename -F press=OK http://example.com
```


可以利用 `-T` 或 `--upload-file` 来 PUT 一个文件:

```
curl -T uploadfile http://abc.test.com
```
