created: 2015-11-28T12:07:00+08:00
tags: [linux, archlinux]


## 制作 USB 安装盘

从 [Arch 官网](https://www.archlinux.org/download/)上下载 archlinux 安装 iso 格式镜像
（文件名类似于 archlinux-2015.11.01-dual.iso）。

```
wget http://mirrors.163.com/archlinux/iso/2015.11.01/archlinux-2015.11.01-dual.iso
```

校验 iso 文件：

```
md5sum archlinux-2015.11.01-dual.iso
```

如果校验 md5 一致，可将 iso 镜像文件写入 usb 安装盘（设备名称为 /dev/sdx）中：

```
dd bs=4M if=/path/to/archlinux.iso of=/dev/sdx && sync
```


## 开机启动

开机，设置从 usb 盘启动，进入安装界面，此时默认用户为 root 用户。


## 磁盘分区

推荐使用 GPT 分区表：

```
cgdisk /dev/sda
```

分区时须选择：

* 分区开始位置
* 分区大小
* 分区类型（EFI，linux root，linux home，linux default，linux swap）

分区方案：

* `/dev/sda1`: /boot EFI 分区，512M
* `/dev/sda2`: swap 分区，两倍内存
* `/dev/sda3`: / linux root 分区，20G
* `/dev/sda4`: /usr linux default 分区，20G
* `/dev/sda5`: /var linux default 分区，50G
* `/dev/sda6`: /tmp linux default 分区，20G
* `/dev/sda7`: /home linux home 分区，剩余大小

格式化分区：

```
mkfs.vfat -F32 /dev/sda1
mkswap /dev/sda2
swapon /dev/sda2
mkfs.ext4 /dev/sda3
mkfs.ext4 /dev/sda4
mkfs.ext4 /dev/sda5
mkfs.ext4 /dev/sda6
mkfs.ext4 /dev/sda7
```


## 挂载分区

```
mount /dev/sda3 /mnt    # 安装系统 root 分区
mkdir -p /mnt/boot /mnt/usr /mnt/var /mnt/tmp /mnt/home
mount /dev/sda1 /mnt/boot
mount /dev/sda4 /mnt/usr
mount /dev/sda5 /mnt/var
mount /dev/sda6 /mnt/tmp
mount /dev/sda7 /mnt/home
```


## 安装基础系统

编辑 `/etc/pacman.d/mirrorlist`，将要使用的源复制到最前面，然后刷新：

```
pacman -Syy
```

安装 `base` 和 `base-devel`：

```
pacstrap /mnt base base-devel
```


## 生成 fstab

```
genfstab -U -p /mnt >> /mnt/etc/fstab
cat /mnt/etc/fstab
```


## 切换到新安装系统

```
arch-chroot /mnt /bin/bash
```


## 设置语言地区

编辑 `/etc/locale.gen`，去除下面行的注视：

```
en_US.UTF-8 UTF-8
zh_CN.GB18030 GB18030
zh_CN.GBK GBK
zh_CN.UTF-8 UTF-8
zh_CN GB2312
```

运行：

```
locale-gen
```

编辑 `/etc/locale.conf` 为：

```
LANG=en_US.UTF-8
```


## 设置终端字体

编辑 `/etc/vconsole.conf`：

```
KEYMAP=us
FONT=Lat2-Terminus16
```


## 设置时区

```
ln -s /usr/share/zoneinfo/Asia/Shanghai /etc/localtime
```


## 设置时间

```
timedatectl set-time "yyyy-MM-dd hh:mm:ss"
```


## 设置主机名

编辑 `/etc/hostname`：

```
HOSTNAME
```


## 添加用户，设置密码

设置 root 密码：

```
passwd
```

添加 user 用户：

```
useradd -G wheel user
passwd user
chfn user
mkdir -m 700 /home/user
chown user:user /home/user
```


## 生成初始 ramdisk 环境

如果 `/usr` 和 `/` 不在同一个分区，
须修改 `/etc/mkinitcpio.conf` 文件，
HOOKS 行须添加 `usr fsck shutdown` 内容。

运行：

```
mkinitcpio -p linux
```


## 安装引导装载程序

```
bootctl install
```

编辑 `/boot/loader/entries/arch.conf`：

```
title          Arch Linux
linux          /vmlinuz-linux
initrd         /initramfs-linux.img
options        root=PARTUUID=xxxxxxxxxxxxx rw
```

`options root` 项对应系统 root 分区，其中 `PARTUUID` 可以由命令 `blkid` 获得。

编辑 `/boot/loader/loader.conf`：

```
timeout 1
default arch
```


## 设置网络

### 有线

```
systemctl enable dhcpcd
```


## 退出安装系统并重启

```
exit
umount -R /mnt
reboot
```


## 安装 X 窗口管理系统

```
pacman -S xorg-server xorg-xinit xorg-utils xorg-server-utils
```


## 通用显卡驱动

```
pacman -S xf86-video-vesa
```


## 声卡驱动

```
pacman -S alsa-utils
```


## 安装字体

```
pacman -S ttf-dejavu wqy-zenhei wqy-microhei
```


## 安装 fcitx 输入法

```
pacman -S fcitx fcitx-qt4 fcitx-qt5 fcitx-gtk2 fcitx-gtk3
```

编辑 `~/.xinitrc` 添加：

```
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
```


## 安装 chromium

```
pacman -S chromium
```


## 安装 pacaur

从 archlinux aur 网站下载 pacaur PKGBUILD，运行：

```
makepkg
pacman -U 生成的文件，以.xz结尾
```

`makepkg` 时会报 gpg 验证出错，需要运行：

```
gpg --recv-keys xxxxxxxxID
```
