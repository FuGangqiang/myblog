date: 2015-08-15 12:03:00
tags: markdown


## mdbook

`mdbook` 是一个类似与 `gitbook` 的命令行工具，使用 Markdown 格式文件自动生成书籍文档，
而构建 `mdbook` 的语言是 `rust`，其源码存放于 [github](https://github.com/azerupi/mdBook)，
而其[自身文档](http://azerupi.github.io/mdBook/)就是 `mdbook` 自动生成的。


## 编译 mdbook

编译之前，系统中需要安装 `rustc` 和 `cargo`，
`rustc` 是 rust 语言的编译器，
而 `cargo` 是用来管理 rust 源码下载、编译、构建的工具。

下载 `mdbook` 源码：

```
git clone --depth=1 https://github.com/azerupi/mdBook.git
```

编译源码：

```
cd mdBook
cargo build --release
```

此时可执行文件 `mdbook` 就会出现在 `./target/release` 文件夹下，
应将此文件夹放入环境变量 `PATH` 内。


## 运行 mdbook


### init 子命令

运行 `init` 子命令如下：

```
cd book-test
mdbook init
```

也可以初始化指定文件夹：

```
mdbook init /path/to/book-test
```

这样 `mdbook` 会自动生成一个如下的目录结构：

```
book-test/
├── book
└── src
    ├── chapter_1.md
    └── SUMMARY.md
```

其中 `src` 文件夹就是用来存放 markdown 格式文档，
`book` 文件夹就是用来存放自动生成的文档，
`SUMMARY.md`文件是一个最重要的文件，它是用来定义书籍文档的框架结构的，用来建立书籍文档的索引及其链接文件。


### build 子命令

`build` 子命令就是用来渲染书籍文档的：

```
cd book-test
mdbook build
```

也可以渲染指定文件夹：

```
mdbook build /path/to/book-test
```

这样，`mdbook` 就会解析 `SUMMARY.md` 文件，来了解书籍文档的框架，获取链接文件，渲染相应 markdown 文件。


### SUMMARY.md 格式

允许元素：

* `Title`：通常是 `# Summary` 这种形式，它只是用来代表一个标题，解析时 `mdbook` 命令会忽略该行
* `list link`：列表元素，用来构建书籍文档的框架结构，形式如下：

        - [Title of the Chapter](relative/path/to/markdown.md)

示例如下：

```
# Summary

- [mdBook](README.md)
- [Command Line Tool](cli/cli-tool.md)
    - [init](cli/init.md)
    - [build](cli/build.md)
- [Format](format/format.md)
    - [SUMMARY.md](format/summary.md)
    - [Configuration](format/config.md)
    - [Theme](format/theme/theme.md)
        - [index.hbs](format/theme/index-hbs.md)
        - [Syntax highlighting](format/theme/syntax-highlighting.md)
- [Rust Library](lib/lib.md)
```


## 博客相关

本博客最近打算用 `mdbook` 来整理以前记录的一些笔记文档，
目前尚处于初始阶段，不过以后会逐步完善笔记的。

笔记索引存放于 [notes](http://fugangqiang.github.io/blog/posts/notes.html) 页面下。
