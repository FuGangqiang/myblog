created: 2015-12-15T12:56:58+08:00
tags: [git]


在 GitHub 上利用 Pull Request 功能，可以很方便地向其他项目贡献代码。本文简要介绍其操作流程。


## Fork 项目 repo

在 GitHub 上点击其他人项目 repo 的 `Fork` 按钮之后，就能在自己账户下看到同名的新 fork 的 repo。


## Clone 项目到本地

在自己帐号下新生成的 repo 中找到 clone url，然后运行：

```
git clone repo_clone_url
```


## 添加远程分支

在项目原始仓库中找到 clone url，在本地 git repo 中添加 `upstream` 远程分支：

```
git remote add upstream origin_repo_clone_url
```

通过 upstream 就可以通过以下命令使本地仓库更新到最新版本，与原始仓库保持一致。

```
git fetch upstream
git merge upstream/master
```

## 创建新分支并修改代码

在新创建的分支下进行修改代码：

```
git checkout -b hotfix
emacs file
git -am "some changes"
```


## 推送新建分支至 GitHub

把本地新建分支推送到自己的 GitHub 帐号下的相对应 repo：

```
git push origin hotfix
```


## 提交 Pull Request 并等待合并

自己对应项目中会有一个绿色的 `pull request` 绿色按钮，点击此按钮即可 pull request 了。


## 删除本地分支

当原项目合并你贡献代码后，就可以删除你的本地分支了：

```
git checkout master
git fetch upstream
git merge upstream/master
git branch -d hotfix
git push origin :hotfix
```
