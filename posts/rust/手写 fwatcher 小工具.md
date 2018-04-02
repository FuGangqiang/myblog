created: 2017-02-20T19:15:41+08:00
tags: [rust, 自动化]


在 web 开发过程中，我们经常需要在某个或某些文件被修改后运行一些命令。

例如：在 django web 开发时，当运行 `manage.py runserver` 后，
如果我们修改了 django 工程相关的代码，`manage.py` 程序会在后台自动重启服务，
使代码即时生效，以便于检验代码正确性。

然而在 python 众多 web 框架中，许多都没有 django 这种类似功能，在其他语言也同样如此。
而且这种当某些文件修改后及时运行某些指令的情况很常见，
因此，自己决定写一个类似的监控文件的小程序 `fwatcher`，以便以后的自动化开发。


fwatcher 用 rust 语言编写，
不仅可以直接用于命令行，
也可以通过 crate lib 来集成到 rust 程序中，以便于其他 rust 应用调用。

代码库 repo: [https://github.com/FuGangqiang/fwatcher.rs](https://github.com/FuGangqiang/fwatcher.rs)


## fwatcher 命令行


#### 安装

需要 rust 的 cargo 包管理器：

```sh
cargo install fwatcher
```

这样 `fwatcher` 就被安装在你的 `~/.cargo/bin` 目录中了。


#### 用法

```sh
$ fwatcher -h
Usage:
    fwatcher [options] CMD

Options:
    -h, --help          Display this message
    -v, --version       Print version info
    -r, --restart       Auto restart command
    -d, --directory <dir>
                        Watch directory, default to current directory
    -p, --pattern <pattern>
                        Watch file glob pattern, default to "*"
    -i, --interval <pattern>
                        Interval in seconds to scan filesystem, default to 1
```

如果你想在当前文件夹下递归搜索 python 文件，当其中一个被修改了，就立即运行 `pytest` 程序：

```sh
fwatcher -p "**/*.py" "pytest"
```

当然，你也可以指定只监控哪些文件夹：

```sh
fwatcher -d src -d test -p "**/*.py" "pytest"
```

上面命令每检测到一个 python 文件被修改，就会运行 `pytest` 程序一次，即使上一次修改导致的 `pytest` 还再运行。

当运行的程序比较耗时，又或者是一个服务并不会停止时，你如果需要重启上一次开启的程序，
这时需要加 `--restart` 参数：

```sh
fwatcher -p "**/*.py" --restart "run_forever_cmd"
```

## Rust Lib

Cargo.toml 依赖：

```toml
[dependencies]
glob = "0.2"
notify = "4.0"
fwatcher = "*"
```

下面是一个示例：

```rust
extern crate glob;
extern crate fwatcher;

use fwatcher::Fwatcher;
use glob::Pattern;
use std::path::PathBuf;
use std::time::Duration;


fn main() {
    let dirs =vec![PathBuf::from("src")];
    let cmd = vec!["pytest".to_string()];

    let mut fwatcher = Fwatcher::new(dirs, cmd);
    fwatcher.pattern(Pattern::new("**/*.py").unwrap())
            .exclude_pattern(Pattern::new("**/.git/**").unwrap())
            .interval(Duration::new(1, 0))
            .restart(false)
            .run();
}
```
