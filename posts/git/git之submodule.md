created: 2016-08-30T12:45:15+08:00
tags: [git]


最近在重构自己的博客，遇到一个问题，用到了 git 的 submodule 功能了。

先说明一下我的问题，我的 Github 静态博客顶层 url 主要有三种：

* `/blog`: 一些技术博客文章
* `/notes`：一些平时记得一些小笔记
* `/docs`：自己写的代码工程文档

这三种类别的静态页面是通过不同的技术生成的：

* `/blog`: 由自写的 [`mdblog`](https://github.com/FuGangqiang/mdblog.rs) 静态博客生成程序
* `/notes`: 由 [`mdbook`](https://github.com/azerupi/mdBook) 文档生成程序
* `/docs`: 由各种杂七杂八的程序或编程语言自动构建

这些 markdown 源文档或程序是分开保存在我的 git 仓库中的，
最终生成的静态 html 也是分开的，所以需要一些自动化来收集这些静态 html，
一开始，我是用 `shell` 脚本简单的复制目录，感觉很不方便、优雅，
后来想到了用 git 的 `submodule` 功能来实现这个功能了。


## 向 repo 中添加 submodule

```
cd repo
git submodule add https://submodule.url/bar.git
git commit -am 'add submodule'
```

此时在 repo 顶层目录下多了一个 `.gitsubmodule` 文件。



## 复制带有 submodule 的 repo

```
git clone https://repo.url/foo.git
git submodule init
git submodule update
```

或者

```
git clone --recursive https://repo.url/foo.git
```


## 同步所有 submodule

```
git submodule foreach git pull
```

## 修改 submodule 的 url

直接修改 .gitsubmodule 中对应的 url，然后运行：

```
git submodule sync
```
