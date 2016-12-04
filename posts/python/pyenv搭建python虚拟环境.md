date: 2016-12-04 15:43:47
tags: python

在 python 开发过程中，经常同时开发多个程序，
其各自依赖的 python 版本号不同，又需要各自特定版本的依赖库，
这样，我们在开发过程中需要来回切换不同版本的 python 以及 python virtualenv，
这样我们就需要一个即有管理 python 版本，又有管理 python virtualenv 的应用，
[`pyenv`](https://github.com/yyuu/pyenv)即是为此。


## 安装 pyenv


```
curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash
```

所有相关文件均被安装到 `~/.pyenv` 文件夹中，

当然卸载 `pyenv` 及其安装的一切 python 版本及 virtualenv 只需删除 `~/.pyenv` 文件夹即可。

```
rm -fr ~/.pyenv
```


## 配置 pyenv

可以在你的 `.bashrc` 配置文件里面添加如下：

```
export PATH="~/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
```

这样在每次启动终端时，你都可以用 `pyenv` 了。


## 更新 pyenv

利用 `update` 命令可以更新到最新版本：

```
pyenv update
```


## 安装、查看及卸载 python 特定版本

查看 `pyenv` 支持安装的 python 版本：

```
pyenv install -l
```

安装 `python 3.5.2` 版本：

```
pyenv install 3.5.2
```

所有相关安装程序均被安装在 `~/.pyenv/versions` 文件夹下。

卸载 python 版本：

```
pyenv uninstall 3.5.2
```

查看 `pyenv` 下所有安装的 python 版本：

```
pyenv versions
```

其中：

* 以 `*` 开头的为当前 pyenv 设置的 python 版本
* system 版本为当前操作系统中存在的版本，而不是由 pyenv 安装的

查看当前 python 版本：

```
pyenv version
```


## 切换 python 版本


当在终端执行 python 时，`pyenv` 按以下顺序来决定使用哪一个 python 版本：

* `PYENV_VERSION` 环境变量，可以用 `pyenv shell` 设置此环境变量
* 当前文件夹下的 `.python-version` 文件（如果存在），可以用 `pyenv local` 添加或修改此文件
* 当前文件夹向上逐级查找父文件夹，直至根目录，第一个被发现的 `.python-version` 文件
* 文件 `~/.pyenv/version`，可以用 `pyenv global` 修改此文件

你也可以同时激活多个 python 版本，这样你就可以使 python2 和 python3 版本共存，比如：

```
pyenv global 3.5.2 2.7.12
```

可以通过 `pyenv which` 来查看 python 某个版本的可执行文件路径。


## 创建、激活、停用及删除 virtualenv

创建名为 `venv` 的 virtualenv 环境：

```
pyenv virtualenv 3.5.2 venv
```

激活 `venv` 环境：

```
pyenv activate venv
```

停用当前 `venv` 环境：

```
pyenv deactivate
```

删除 `venv` 环境：

```
pyenv virtualenv-delete venv
```


## 其他命令

可以通过 `pyenv commands` 来查看所有 `pyenv` 的子命令：
