created: 2016-02-28T12:47:10+08:00
tags: [git]


你是否会有这样的需求：

>一个项目，这个项目里面包含有数个子项目，
>你希望各个子项目可以单独拥有自己的版本控制（因为子项目可以被重用于其他项目），
>同时你可以时刻将子项目的代码更新到最新版本，
>而后在本项目中修改子项目的代码并提交到子项目中去。

Git 的 submodule 和 subtree 命令就是为满足上面需求而产生的。
而本篇只讲 subtree，因为 subtree 比 submodule 有许多优势：

* 管理便捷
* clone 项目时，子项目相关代码也会被拉取下来，不在需要 git-submodule 那样 init 和 update 了
* 不再需要 .gitmodule 配置文件了
* 不用像 submodule 那样删除子模块费劲了


## 创建 subtree

有两种方式：

* 从无到有：在项目一开始就建立项目和子项目的模型
* 从一到多：项目已部分完成，但是发现其中的一部分代码也可以用于其他项目，想要把它分离出来独立维护


### 从无到有

现在已经有了两个 git 仓库：

```
https://github.com/example/project.git
https://github.com/example/lib.git
```

其中，要在 project 中用到 lib 的代码：

```
git clone https://github.com/example/project.git
cd project
git subtree add --prefix=lib https://github.com/example/lib.git master
```

其中 `https://github.com/example/lib.git` 会在以后维护中用到，我们还可以把它提前保存为 remote 方便以后引用。
其实下面的命令会用到的比较多：

```
git clone https://github.com/example/project.git
cd project
git remote add libremote https://github.com/example/lib.git
git subtree add --prefix=lib libremote master
```


### 从一到多

现在已经有了两个 git 仓库：

```
https://github.com/example/project.git
https://github.com/example/lib.git
```

其中，我们想把 project 中的 lib 目录中的代码分离出来作为一个独立的 repo 维护，
并把它推送到 `https://github.com/example/lib.git` 中：

```
git clone https://github.com/example/project.git
cd project
git subtree split --prefix=lib -b split
git remote add libremote https://github.com/example/lib.git
git push libremote split:master
git rm -r lib
git commit -am "removing lib folder"
git subtree add --prefix=lib libremote master
```


### 拉取、提交 subtree 代码

和 git 的拉取、提交命令相似：

```
git subtree pull --prefix=lib libremote master
git subtree push --prefix=lib libremote master
```
