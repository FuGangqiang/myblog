created: 2015-11-28T12:07:00+08:00
tags: [linux]


## 安装samba

```
pacman -S samba
```


## 配置

samba 配置文件为 /etc/samba/smb.conf，默认没有这个文件，可以复制样本：

```
cp /etc/samba/smb.conf.default /etc/samba/smb.conf
```

配置文件格式类似于 windows 下的 ini 文件格式，形为：

```
[global]
...

[share1]
...

[share2]
...
```

其中第一个 `global` 语句块属于全局设置，下面其他块都属于共享资源设置。
其中以 `#` 或 `;` 开始的行为注释语句。


一个示例：

```ini
[global]
# 设置所在工作组
workgroup = WORKGROUP
＃ 设置工作组中用户名
netbios name = fu
# 设置guest访问
guest account = ftp

[public]
# 设置可访问的public文件夹相对应的samba服务器下文件夹
path = /home/fu/public
# 设置可访问
public = yes
# 设置共享资源只读不可写
read only = yes
writable = no
```

注：如果需要通过用户名和密码验证才可访问共享资源，
需要在 samba 服务器上运行 `smbpasswd username` 命令设置用户名和密码，
其中该用户名必须为 `/etc/passwd` 文件中的用户名（sambda 是通过这个用户名来访问共享资源的）。


## 开启共享服务

```
systemctl start smbd
systemctl start nmbd
```
