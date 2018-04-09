created: 2018-04-09T10:34:00+08:00
tags: [博客, rust]


最近刚好有点空闲，就继续完善 [mdblog.rs][] 项目，
再加上 [rust][] 语言生态最近逐渐完善以及 rust 语言的特点，
重构起来真是特别的舒心，也体验到了静态语言的强大优势。

经过数个不兼容版本的迭代，[mdblog.rs][] 终于有了我想要的所有功能：

* 配置简单
* TeX 数学公式
* 博客 url 为文件路径，而不是日期
* 博客标题为源文件名
* 博客索引页面分页
* 可以设置博客的 base url
* 可以隐藏某些尚不准备发布的博客
* 可以自定义博客 theme 样式
* `serve` 命令监控源文件变化自动构建博客
* `rss/atom` Feed 支持

当然也用了许多 rust 的很优秀 crates：

* `structopt`：替换了原来的 `getopts`
* `failure`：替换了原来的 `error-chain`
* `serde_yaml`：替换了原来的手动解析博客的 header
* `tempdir`：实现了用临时文件夹来存放 `serve` 命令产生的临时静态文件
* `open`：浏览器自动打开 `serve` 命令产生的博客测试首页
* `hyper`：用于 `serve` 命令产生的测试后台 http 服务
* `shellexpand`：自动对博客配置某些目录进行变量或特殊符号的替换功能
* `config`：替换了原来的 `toml`，实现了配置项的分层配置
    - 默认配置
    - `config.toml` 配置
    - 环境变量配置

不过目前还有一些尚未确定是否应该是现有的实现方式，比如：

* 有些配置是否应固定在 `theme` 里还是在 `config.toml` 里面设置
* 有些博客的信息是否应该放到每个页面模板 context 环境中

这些就在以后使用和反馈中在逐渐修改吧。

那么现在就来看一下怎么用 [mdblog.rs][] 来构建博客吧！


## 安装 rustc & cargo

因为 `mdblog.rs` 是由 rust 语言编写的，所以系统中需要安装 rustc 和 cargo，
推荐利用 [rustup.rs][] 来安装它们，目前我只在 nightly 版本测试编译 `mdblog.rs`：

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
│   ├── hello.md
│   └── math.md
└── _themes
    └── simple
```

其中：

* `config.toml`：博客配置文件
* `media`：博客媒体文件夹
* `posts`：博客 markdown 文件夹
    - `hello.md`：测试 markdown 文件
    - `math.md`：测试 TeX math 文件
* `_themes`：博客主题文件夹
    - `simple`：博客预置 simple 主题

## 编写博客文章

```
cd myblog
mdblog new another
```

运行 `new` 命令会创建一个 `posts/another.md` 的博客样例文件：

```
created: 2018-04-09T11:15:22+08:00
tags: [blog]

this is a new post!
```

从上面可以看到，一篇博客分为两部分：

* `header`：博客元信息，yaml 格式
* `body`：博客 markdown 内容

`header` 和 `body` 由第一个空行分割开，其中 `header` 有以下几个配置项：

* `created`：必填项，博客生成日期
* `tags`：选填项，博客标签，默认: `[]`
* `hidden`：选填项，博客是否隐藏，默认：`false`
* `description`：选填项，博客概述，默认：body 的第一段或前 100 个字符

## 测试博客

对刚才新创建的 `posts/another.md` 文件进行一些修改，
就可以检测一下最终的博客产生页面，运行：

```
mdblog serve
```

该命令会自动打开浏览器，并进入博客的索引页面，你就可以点击查看新的博客文章了。

当你再次去修改某篇博客文章时，`serve` 命令会自动检测到你对某些文件进行了修改，
然后重新构建博客静态文件，这样你在浏览器中刷新页面就可以看到这些更新了。


## 构建博客

当你用 `serve` 命令查看博客的测试结果已基本符合你的要求后，就可以正式的构建博客了：

```
mdblog build
```

这样所有的博客静态文件就被写入到 `_build` 文件夹里了。

## 配置博客

[mdblog.rs][] 提供了许多可供自定义配置的选项，
可以通过 `config.toml` 文件查看都是有哪些可配置项以及它们的默认值：

```
site_url = ""
site_name = "Mdblog"
site_motto = "Simple is Beautiful!"
footer_note = "Keep It Simple, Stupid!"
media_dir = "media"
build_dir = "_build"
theme = "simple"
theme_root_dir = "_themes"
rebuild_interval = 2
posts_per_page = 20
```

其中 `media_dir`、`build_dir`、`theme_root_dir` 是目录路径设置，
可以利用 shell 环境变量来指定路径，比如：

* `$HOME/blog/media`
* `~/blog/build`

## 自定义博客主题

可以通过自定义博客主题来修改你的博客样式，首先，你需要创建一个博客主题：

```
mdblog theme new mytheme
```

这样就在 `_themes` 文件夹下生成了和 `simple` 一样的名为 `mytheme` 的主题，
然后你就可以对这个主题进行各种各样的修改了，
不过修改博客主题需要你熟悉：

* html/js/css
* [tera][] 模板，类似于 python 中的 Jinja2

修改后你就可以应用这个主题了：

```
mdblog theme set mytheme
```

或者直接修改 `config.toml` 中的 `theme` 选项就可以了。


## CLI 帮助

```
$ mdblog -h
mdblog 0.12.0
FuGangqiang <fu_gangqiang@qq.com>
static site generator from markdown files

USAGE:
    mdblog <SUBCOMMAND>

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

SUBCOMMANDS:
    build    Build the blog static files
    help     Prints this message or the help of the given subcommand(s)
    init     Initialize the blog directory layout
    new      Create a blog post
    serve    Serve the blog, rebuild on change
    theme    Blog theme operations
```

对子命令也可以查看：

```
$ mdblog theme -h
mdblog-theme 0.12.0
FuGangqiang <fu_gangqiang@qq.com>
Blog theme operations

USAGE:
    mdblog theme <SUBCOMMAND>

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

SUBCOMMANDS:
    delete    Delete a theme
    help      Prints this message or the help of the given subcommand(s)
    list      list blog themes
    new       Create a new theme
    set       Set blog use the theme
```

[rust]: https://www.rust-lang.org/en-US/
[mdblog.rs]: https://github.com/FuGangqiang/mdblog.rs
[markdown]: http://commonmark.org/
[TeX]: https://en.wikipedia.org/wiki/TeX
[rustup.rs]: https://rustup.rs/
[tera]: https://tera.netlify.com/docs/installation/

