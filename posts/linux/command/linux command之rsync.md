created: 2014-12-17T20:11:14+08:00
tags: [linux, CLI]


## Example

* source 和 destination 可以有一个时远程文件地址
* 其格式为：user@host:/location/to/file

使目的文件夹与原始文件夹完全一样

```
rsync -a --delete source destination
```


排除文件夹

```
rsync -a --exclude=pattern_to_exclude source destination
```
