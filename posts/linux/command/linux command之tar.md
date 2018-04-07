created: 2014-05-11T12:00:06+08:00
tags: [linux, CLI]


`tar`(tape archive，磁带归档) 命令是常用的打包命令（将一组文件打包转换成 `.tar` 格式的包），
尽管最初设计被用来作为磁带备份的，但是现在它可以在任何文件系统上打包。

一个 `.tar` 包由任意数量的文件组成，附带一些目录元数据，用来在解包时还原这些文件的原有结构。
`.tar` 包经常被用来传递、发布一组文件，许多开源项目的源码均是 `.tar` 格式的。

`tar` 命令的基本用法如下：

```
tar option(s) archive_name file_name(s)
```


## 打包

生成一个 `.tar` 包：

```
tar -cvf file.tar file1 file2 file3
```

其中：

* `-c`：生成 `.tar` 包
* `-v`：显示被打包的文件
* `-f`：紧接着的参数为 `.tar` 包文件名，尽管文件名可以不以 `.tar` 结尾，但是用它结尾可以标识一个文件是 `.tar` 包


`tar` 也可用于打包文件夹：

```
tar -cvf dir.tar dir1 dir2
```

在打包时，源文件是不变的，如果想要在打包时删除源文件，可以用 `--remove-files` 选项。
如果在打包某文件夹时想要忽略某文件，可以用 `--exclude=/path/to/exclue/file` 选项。


## 解包

解开一个 `.tar` 包：

```
tar -xvf file.tar
```

其中：

* `-x`：解开 `.tar` 包


可以用 `--extract` 选项只解出一个文件。


## 修改包

向 `.tar` 包中添加文件：

```
tar -rf file.tar file4
```

在 `.tar` 包中删除某文件：

```
tar -f file.tar --delete file4
```


## 包目录

列出 `.tar` 包中的文件目录：

```
tar -tf file.tar
```


## 压缩包


压缩为 `.tar.gz` 或者 `.tgz` 文件：

```
tar -cvzf file.tar.gz file1 file2 file3
```

其中：

`-z`：通过 `gzip` 压缩包


压缩为 `.tar.bz2` 文件：

```
tar -cvjf file.tar.bz2 file1 file2 file3
```

其中：

`-j`：通过 `bzip2` 压缩包



压缩为 `.tar.xz` 文件：

```
tar -cvJf file.tar.xz file1 file2 file3
```

其中：

`-J`：通过 `xz` 压缩包


## 解压缩包

同压缩包相似，只是将 `-c` 选项替换为 `-x` 选项：


```
tar -xvzf file.tar.gz
tar -xvjf file.tar.bz2
tar -xvJf file.tar.xz
```
