created: 2015-12-30T12:54:03+08:00
tags: [git]


* blank lines or lines starting with `#` are ignored
* stardard glob patterns work
    * `*` matches zero or more characters
    * `[abc]` matches any character inside the brackets
    * `[0-9]` matches any character between them
    * `a/**/z` matches `a/z`, `a/b/z`, `a/b/c/z`, and so on
* negate a pattern by starting it with an `!`

示例：

```
*.a     # 所有已 .a 结尾的文件
!lib.a  # 将跟踪 lib.a，即使 *.a
/TODO   # 仅忽略顶层TODO，而不是 subdir/TODO
build/  # 忽略build/文件夹下所有文件
doc/*.txt     # 忽略 doc/notes.txt，但不忽略doc/server/arch.txt
doc/**/*.txt  # 忽略所有doc/文件夹下的所有 .txt 文件，包括doc/*.txt
```
