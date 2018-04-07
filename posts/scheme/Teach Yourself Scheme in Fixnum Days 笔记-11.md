created: 2010-12-27T00:43:00+08:00
tags: [scheme]

# 第十一贴：系统接口

通常一些有用的 scheme 程序需要与所在的操作系统交流
 
## Checking for and deleting files
 
`file-exists?` 过程检查其参数<字符串>所指的文件是否存在；
`delete-file` 删除其参数<字符串>所指的文件。
这些过程并不是 scheme 标准的一部分，但是大部分 scheme 实现都包含了它们。
 
`stat` 过程返回其参数对象的状态值，可以用这个返回的状态值调用以下过程得到自己想要的数据：

* `stat:dev`
* `stat:ino`
* `stat:mode`
* `stat:nlink`
* `stat:uid`
* `stat:gid`
* `stat:rdev`
* `stat:size`
* `stat:atime`
* `stat:mtime`
* `stat:ctime`
* `stat:atimesec`
* `stat:mtimesec`
* `stat:ctimesec`
* `stat:blksize`
* `stat:blocks`
* `stat:type`
* `stat:perms`

## Calling operating-system commands

`system` 过程通过操作系统执行其字符串参数。

```
(system "ls")
;lists current directory
 
(define fname "spot")
(system (string-append "test -f " fname))
;tests if file 'spot' exists
```

最后一个语句与下面等同：

```
(file-exists? fname)
(delete-file fname)
```

## Environment variables

`getenv` 过程返回操作系统的环境变量：

```
guile>(getenv "HOME")
"/home/fu"
guile>(getenv "SHELL")
"/bin/bash"
```
