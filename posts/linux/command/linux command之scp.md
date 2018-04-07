created: 2014-12-14T22:02:34+08:00
tags: [linux, CLI]


## Example

复制远程主机中 foobar.txt 文件至本地目录

```
scp your_username@remotehost.edu:foobar.txt /some/local/directory
```


复制本地目录文件 foobar.txt 至远程主机某目录

```
scp foobar.txt your_username@remotehost.edu:/some/remote/directory
```


复制本地目录 foo 至远程主机某目录

```
scp -r foo your_username@remotehost.edu:/some/remote/directory/bar
```
