created: 2014-05-23T21:07:06+08:00
tags: [linux, CLI]


## Example

镜像网站

```
wget --mirror -p --convert-links -D dom -P dir url
```

* --miror开户镜像下载
* -p下载所有为了html页面显示正常的文件
* --convert-links下载后，转换成本地的链接
* -P ./LOCAL保存所有文件和目录到本地指定目录
* -D 制定域名
