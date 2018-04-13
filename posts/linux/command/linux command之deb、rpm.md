created: 2014-06-24T13:21:13+08:00
tags: [linux, CLI]



## 查看某个包中安装的文件列表 deb

```
dpkg -L pkg_name
```


## 查看系统中某个文件属于哪个包

```
dpkg -S /file/path
```


## 解压 deb 包

如果系统中有 `dpkg-deb` 命令，可以用下面命令解压：

```
dpkg-deb --fsys-tarfile filename.deb | tar xvf -
```

如果没有 `dpkg-deb` 命令，可以使用如下命令：

```
ar -x fileName.deb
```

解压完毕后会出现几个文件，主要用到 `data.tar.xz` 包，
这个是存放二进制文件的压缩包，一般我们需要的文件都在里面，用命令解压：

```
tar -Jxvf data.tar.xz
```


## 解压 rpm 包

运行如下命令：

```
rpm2cpio xx.rpm | cpio -idmv
```

