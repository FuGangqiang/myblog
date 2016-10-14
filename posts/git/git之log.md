date: 2016-01-14 12:54:47
tags: git


* `--author=pattern`：仅列出某作者的提交
* `--committer=pattern`：仅列出某提交者的提交
* `--abbre-commit`：
* `--name-only`：仅列出改变的文件名
* `--name-status`：列出改变的文件名及状态
* `--stat`：列出文件改变统计信息
* `--shortstat`：列出简短文件改变统计信息
* `--oneline`：将提交信息压缩到一行
* `--graph`：树形显示所有提交
* `--reverse`：反向显示（最早在前）提交
* `--after`：列出某时间点后的所有提交
* `--before`：列出某时间点前的所有提交
* `--pretty`：自定义输出格式
    - `%h`：abbreviated commit hash
    - `%T`：Tree hash
    - `%t`：abbreviated tree hash
    - `%P`：parent hashes
    - `%p`：abbreviated parent hashes
    - `%s`：subject
    - `%an`：author name
    - `%ae`：author email
    - `%ad`：author date
    - `%ar`：author date relative
    - `%cn`：committer name
    - `%ce`：committer email
    - `%cd`：committer date
    - `%cr`：committer date relative
* `-p`：输出详细 diff 改变，pitch
    ```
	git log -p filename
	```
* `-L`：列出特定行的改变
    ```
	git log -L 1,1:some-file.txt
	```
*


```
git log -S <pattern>  # search <pattern> from delete part or add part
git log -p -2    # show patch, limits to last 2 entry
git log --relative-date
git log --pretty=oneline
git log --pretty="%h - %an, %ar : %s"

git log -n
git log --pretty=format:"%h %s" --graph
git log --since=2.weeks  --until=1.weeks
git log --after=2.weeks  --before=1.weeks
git log --grep=pattern
git log --oneline --decorate  # show where the branch pointer are pointing

git log --no-merges master..
git show --no-merges master..
git log -p --no-merges master..
git show some-branch:some-file.js
git pull -—rebase
git merge --no-ff
git diff -w
git add -p
git stash —keep-index
git stash -p
git rm --cached

git log master..exper    # all commits reachable by exper that aren't reachable by master
git log origin/master HEAD

git log <ref1> <ref2> ^<ref3>        # all commits reachable by <ref1> <ref2> but <ref3>
git log <ref1> <ref2> --not <ref3>   # same as above
git log <ref1>...<ref2>     # all commits both reachable by <ref1> and <ref2>
```



* 列出项目所有作者

```
git log --format='%aN %ae' | sort -u
```
