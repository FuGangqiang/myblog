date: 2016-08-25 10:34:00
tags: 博客, rust


最近在学习 [rust][] 语言，
想拿一个项目练一下手，想到了重构自己的静态博客平台，
就着手写了 [mdblog.rs][] 项目，目前已经用它来重构了我的 GitHub 的静态博客。


现在先说一下 mdblog.rs 支持的一些功能：

* 博客文章用 [markdown][] 格式编写
* 支持 [TeX][] 数学公式
* 代码高亮显示
* 博客标签分类
* 隐藏特定博客

还有一些其他特性：

* 博客标题就是文件名
* 博客 url 按照目录结构组建，而不是按照时间


那么来看一下怎么用 mdblog.rs 来构建博客吧！


## 安装 rustc & cargo

因为 mdblog.rs 是由 rust 语言编写的，所以系统中需要安装 rustc 和 cargo，
推荐利用 [rustup.rs][] 来安装它们，目前只能用 nightly 版本编译 mdblog.rs：

```
rustup default nightly
```


## 安装 mdblog.rs

```
cargo install mdblog
```


## 初始化博客文件夹

```
mdblog init myblog
```

此时博客目录树如下：

```
myblog
├── config.toml
├── media
├── posts
│   └── hello.md
└── _themes
    └── simple
        ├── static
        │   ├── css
        │   │   ├── highlight.css
        │   │   └── main.css
        │   ├── img
        │   │   ├── favicon.png
        │   │   └── logo.png
        │   └── js
        │       ├── highlight.js
        │       └── main.js
        └── templates
            ├── base.tpl
            ├── index.tpl
            ├── post.tpl
            └── tag.tpl

9 directories, 12 files
```

## 编写博客文章

博客文章是在 `myblog/posts` 目录里面，也可以在此分类建立文件夹归档博客文章，
每一篇博客文章均由两部分组成：

* `header`
  * `date`: 发布日期，必填项，格式如 `date: 1970-01-01 00:00:00`
  * `tags`: 标签，必填项，格式如 `tags: hello, world`
  * `hidden`: 是否隐藏，可选项，默认为 `false`，格式如 `hidden: true`
* `body`： 博客 markdown 正文

`header` 和 `body` 由第一个空行分割，一片完整的博客文章内容如下(`posts/hello.md`)：

```
date: 1970-01-01 00:00:00
tags: hello, world

# hello

hello world!
```

## 构建博客

进入博客目录：

```
cd myblog
```

开始构建：

```
mdblog build
```

## 测试浏览博客

进入 `myblog/_builds` html 静态页面文件夹：

```
cd _builds
```

搭建服务：

```
python3 -m http.server --bind localhost 8000
```

这样就可以在 [http://127.0.0.1:8000](http://127.0.0.1:8000) 浏览博客页面了。


[rust]: https://www.rust-lang.org/en-US/
[mdblog.rs]: https://github.com/FuGangqiang/mdblog.rs
[markdown]: http://commonmark.org/
[TeX]: https://en.wikipedia.org/wiki/TeX
[rustup.rs]: https://rustup.rs/
