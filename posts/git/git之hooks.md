date: 2016-02-13 12:41:33
tags: git


当你用 Git 管理代码时，
是不是每次提交 commit 前都会重复性的做同一件事情（打开终端，运行各个相关测试命令）?

是不是每次 push 代码到服务器后也重复性的做同一件事情（打开终端，更新代码到服务器，然后 ssh 进服务器，重启相关后台进程）?

......

而 Git 的 hooks 功能就可以帮你自动运行这些手动重复、冗烦的命令。


## Hooks 是什么？

Git 具有在特定事件发生前后执行特定脚本的功能（类似于监听事件、触发器），
Git Hooks 就是那些在 Git 特定事件（如 commit、push、receive 等）前后被触发运行的脚本（shell、python、ruby 等脚本均可）。

这些Hooks脚本位于 `.git/hooks/` 目录(本地和远程 repo 都有)下，
可以在这个目录下自由定制各个 Hooks 脚本，
而当触发一些 Git 行为时，相应地 Hooks 就会被调用执行。


## Hooks的分类

Hooks 脚本按照运行环境可以分为：

* 本地 Hooks，触发事件如 commit、merge 等。
* 服务端 Hooks，触发事件如 receive 等。

按照 Hooks 脚本对应的 Git 特定事件可以分为：

* applypatch-msg

    本地 Hooks，
    由 `git am` 触发，
    而该脚本最先被运行。

* pre-applypatch

    本地 Hooks，
    由 `git am` 触发，
    会在补丁应用后但尚未提交前运行。

* post-applypatch

    本地 Hooks，
    由 `git am` 触发，
    会在补丁应用并提交之后运行。

* pre-commit

    本地 Hooks，
    由 `git commit` 触发，
    在提交前运行，若脚本运行失败（返回非零值），git 提交就会被终止，这个脚本可以通过传递 --no-verify 参数而被禁用。

* prepare-commit-msg

    本地 Hooks，
    由 `git commit` 触发，
    在默认的提交信息准备完成后但编辑器尚未启动之前运行，若脚本运行失败（返回非零值），git 提交就会被终止，这个脚本不会因为 --no-verify参数而被禁用。

* commit-msg

    本地 Hooks，
    由 `git commit` 触发，
    用来验证提交说明的规范性，若脚本运行失败（返回非零值），git 提交就会被终止，这个脚本可以通过传递 --no-verify 参数而被禁用。

* post-commit

    本地 Hooks，
    由 `git commit` 触发，
    在提交完成后运行，不会影响 commit 的运行结果。

* pre-rebase

    本地 Hooks，
    由 `git rebase` 触发，
    在 rebase 执行之前运行。

* post-checkout

    本地 Hooks，
    由 `git checkout` 或 `git clone` 触发（除非在 clone 时使用参数 --no-checkout），
    在完成工作区更新之后运行。

* post-merge

    本地 Hooks，
    由 `git merge` 或 `git pull` 触发，
    在 merge 成功后运行。

* pre-push

    本地 Hooks，
    由 `git push` 触发，
    在本地推送前运行。

* pre-receive

    服务端 Hooks，
    由 `git-receive-pack` 触发，
    在从本地完成一个推送后，远端服务器开始批量更新之前运行。

* update

    本地 Hooks，
    由 `git-receive-pack` 触发，
    和 pre-recieve 类似，只是它会为推送过来的更新中涉及到的每一个分支都运行一次，而后者只运行一次。如果运行失败（返回非零值），相关引用会被拒绝，但其他正常的引用更新都会被接受。

* post-receive

    服务端 Hooks，
    由 `git-receive-pack` 触发，
    在从本地完成一个推送后，远端服务器所有引用都更新完毕后运行。

* post-update

    服务端 Hooks，
    由 `git-receive-pack` 触发，
    和 post-receive 类似，只是它会为推送过来的更新中涉及到的每一个分支都运行一次，而后者只运行一次。

* pre-auto-gc

    本地 Hooks，
    由 `git gc --auto` 触发，
    在每次自动 gc 之前运行。

* post-rewrite

    本地 Hooks，
    由 `git commit --amend` 或 `git rebase` 触发，
    在每次重写 commit 之后运行。


## Hooks 的示例


### 应用一：本地 Hooks

我的博客生成程序的源代码都存储在 [Github myblog repo](https://github.com/FuGangqiang/myblog) 中，
博客文章都是 Markdown 格式的，
而每次有新文章或改进某文章后，都会 push 到这个 repo 上，
同时运行相应的 `./update.sh` 命令来生成博客的 html/css，
并部署在 [Github fugangqiang.github.io repo](https://github.com/FuGangqiang/fugangqiang.github.io) 上，
每次都会执行同样的操作，是不是很麻烦，为了避免这些重复性的动作，我就用了 Git 的 Hooks 来帮我自动完成以上动作。

因为每次都是在本地 myblog repo 执行 `git push` 后，运行以上命令，我就选用了 pre-push hooks 脚本来完成以上任务。

首先，在本地 myblog repo 的 .git/hooks 目录新建一个名为 pre-push 的可执行文件：

```
cd .git/hooks
touch pre-push
chmod +x pre-push
```

接着就可以编写 pre-push hooks 内容如下：

```sh
#!/bin/sh

GITDIR=~/Git
echo "=====>> START blogging <<====="

cd "${GITDIR}/fugangqiang.github.io" && rm -r $(ls)
cd "${GITDIR}/myblog"
./update.sh
cd "${GITDIR}/fugangqiang.github.io"
git add . && git commit -m "automatic build" && git push

echo "=====>> End blogging <<===== "

exit 0
```

完成编辑后，就可以在博客源码更新后，只运行 `git push` 命令来自动更新我的 [Github 博客](http://fugangqiang.github.io/)了。


### 应用二：服务端 Hooks

进行 Web 开发时，
每当有 release 版本时都会把它部署到相应的服务器上，
或者每当有一个 hotfix 完成后，也要更新到相应的服务器上，
一般我们会把代码 rsync 到服务器上，然后再 ssh 进服务器重启相关后台进程。

这些每次都会运行相同命令，而 post-receive hooks 就可以帮助我们来自动完成这些相关重复性动作。


#### git bare repo

首先，我们在服务器上建立一个 git repo，
这样每当我们代码更新时，我们就可以运行 `git push` 命令将代码更新到服务器上：

```
mkdir proj.git
cd proj.git
git init --bare
```

其次，在服务器的 git repo 中生成 post-receive hooks：

```
cd hooks
touch post-receive
chmod +x post-receive
```

编辑其内容如下：

```sh
echo "=====>> Deployment Start <<====="

while read oldrev newrev refname
do
    if [ $refname = "refs/heads/master" ]; then
        echo "STARTing [$refname]"
        git --work-tree=/var/www/html --git-dir=/path/to/proj.git checkout -f
        # restart the service
    fi
done
echo "=====>> Deployment End <<====="
```

最后，在本地添加服务器 remote：

```
git remote add upstream ssh://user@ip:port/path/to/proj.git
```

这样就可以在本地运行 `git push upstream master` 后，自动把代码更新和部署到服务器上了。


#### git normal repo

与上面差不多，只是 git 仓库不是 bare 仓库：

```
mkdir proj
cd porj
git init
git config receive.denycurrentbranch ignore
cd .git/hooks
touch post-receive
chmod +x post-receive
```

编辑 post-receive 内容如下：

```sh
#!/bin/sh

GIT_WORK_TREE=/path/to/proj git checkout -f
```
