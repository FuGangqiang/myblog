created: 2014-04-09T16:05:12+08:00
tags: [linux, CLI]


`find` 命令在目录结构中递归搜索文件，并执行指定的操作，提供了相当多的查找条件，功能很强大。


## Tests

### name

利用模式匹配文件名：

* `*`：匹配任意字符串
* `?`：仅匹配单个字符
* `[]`：匹配某个集合里的字符

```
find . -name "*.log"
find . -name "[A-Z]*"
find . -name "[a-z]?[4-9].log"
```


### path

利用模式匹配路径名

```
find . -path ".src/*/misc
```


### regex

同 path，只是匹配时用正则表达式


### type

按照文件类型查找：

* `b`：块设备文件
* `c`：字符设备文件
* `d`：文件夹
* `p`：pipe 文件
* `f`：普通文件
* `l`：软链接
* `s`：socket 文件
* `D`：door文件(Solaris)

```
find /etc -type d
find /etc -type f
```


### perm

按照文件权限查找

```
find . -perm 755          # 权限为 755
find . -perm g=w          # 权限为 020
find . -perm -220       # 权限至少为 220
find . -perm -g+w,u+w   # 同上
find . -perm /220       # 用户或组其中有一个有写权限
find . -perm /u=w,g=w   # 同上
```


### user/nouser

按照用户查找

```
find . -user root
find . -user 1000
find /home -nouser  # 用户已经被删除的文件
```


### group/nogroup

按照用户组查找，同 user/nouser


### atime/amin/anewer

按照 last accessed time 查找

其中：

* `atime` 时间单位为`天`
* `amin` 时间单位为`分钟`
* `anewer` 与制定文件的 last accessed time 比较

```
find . -atime -2  # 两日以内
find . -atime +2  # 两日以前
find . -anewer /path/to/file   # last accessed time 比制定文件新
```


### ctime/cmin/cnewer

按照 file's status last changed time 查找

同 atime/amin/anewer


### mtime/mmin/newer

按照 file's data last modified time 查找

同 atime/amin/anewer


### size

按照文件大小查找

单位：

* `b`：blocks
* `c`：bytes
* `w`：two-byte words
* `k`：kilobytes
* `M`：megabytes
* `G`：gigabytes

```
find . -size 100c     # 文件大小恰好为 100 bytes
find . -size -100c    # 文件大小小于 100 bytes
find . -size +100c    # 文件大小大于 100 bytes
```


### fstype

按照文件系统类型查找

```
find . -fstype nfs
```


## Operators

* `( expr )`：优先级调整
* `! expr`：非
* `-not expr`：同上
* `expr1 expr2`：并
* `expr1 -a expr2`：同上
* `expr1 -and expr2`：同上
* `expr1 -o expr2`：或
* `expr1 -or expr2`：同上
* `expr1 , expr2`：同上
