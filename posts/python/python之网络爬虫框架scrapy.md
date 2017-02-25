date: 2017-02-25 18:19:40
tags: python, web, 爬虫


最近自己写了一个爬虫程序，用 scrapy 来爬取网上的几十万首诗词，
非常方便，本文总结一下。


## 什么是网络爬虫？

现在已经步入互联网时代，我们接触的大多数信息都可以通过互联网来获取，
最基本的就是通过浏览器，键入网页地址，浏览网站，获取网站信息。

用户可以自行手工上网筛选信息的，当然这样效率是异常的低，
而网络爬虫通过程序自动化来自动访问网页链接，
自动分析网页内容，自动筛选出需要的信息，并将产生的信息数据加以保存起来，
进而大大提高了获取信息的效率。

爬虫程序之所以可以做到自动化，
皆因互联网一切网页都是符合相关标准协议的，
只需按照标准协议来解析网页数据，我们就可以结构化网页数据，并提取相关信息。

由于爬虫程序的结构、功能大多类似，
通过爬虫框架无疑会将我们从爬虫实现的细节中解脱出来，
我们只需关注怎么筛选出需要的数据就可以了，
本文讲解爬虫框架中最常见的 scrapy。


## scrapy 框架

### 安装 scrapy：

```
pip install scrapy
```


### 创建爬虫工程

scrapy 框架与 python django web 框架理念相似，
提供了相似的命令行界面：

```
scrapy startproject tutorial
```

这样 scrapy 就创建了 `tutorial` 爬虫工程文件目录，
里面初始化了爬虫所需的基本文件，
从中可以看出 scrapy 爬虫程序的结构：

```
$ tree tutorial
tutorial
├── scrapy.cfg
└── tutorial
    ├── __init__.py
    ├── items.py
    ├── middlewares.py
    ├── pipelines.py
    ├── __pycache__
    ├── settings.py
    └── spiders
        ├── __init__.py
        └── __pycache__

4 directories, 7 files
```

而我们写的相关爬虫程序只需在 `spiders` 目录里添加相关代码就可以了，
其他文件都是 scrapy 为了更好的扩展而准备的，此时可以不必理这些文件。


### 自定义爬虫程序

创建 `tutorial/tuorial/spiders/quotes.py` 文件：

```
import scrapy


class QuotesSpider(scrapy.Spider):
    name = "quotes"
	start_urls = [
        'http://quotes.toscrape.com/page/1/',
    ]

    def parse(self, response):
        for quote in response.css('div.quote'):
            yield {
                'text': quote.css('span.text::text').extract_first(),
                'author': quote.css('small.author::text').extract_first(),
                'tags': quote.css('div.tags a.tag::text').extract(),
            }
```

从上面代码可以看出，我们只需指定：

* `name`: 爬虫对应的 name，用于唯一标记爬虫程序
* `start_urls`: 爬虫开始的第一个 url 地址
* `parse`: 抓取页面中需要的信息

其他与爬虫相关的所有细节，scrapy 已经提供好了，我们只需写这些就可以正是获取信息了。


### 运行爬虫程序

命令须在爬虫工程目录运行：

```
scrapy crawl quotes -o quotes.json
```

这样，所有结果就保存在 `quotes.json` 文件里了。


### 调试爬虫

为了更好的实时调试，查看代码作用，scrapy 提供了 `shell` 子命令：

```
scrapy shell 'http://quotes.toscrape.com/page/1/'
```

这样就进入了一个命令交互界面，可以运行相关 python 代码实时验证代码。


### 其他

上面只是一个简单的介绍，scrapy 提供了更多的抽象：

* Item
* ItemLoader
* ItemPipeline
* Middleware
* Setting
* Logging
* ...

具体请看 [scrapy 文档](https://doc.scrapy.org/en/latest/)
