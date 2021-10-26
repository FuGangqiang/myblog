created: 2015-11-28T12:07:00+08:00
tags: [linux, archlinux]


## 制作 USB 安装盘

从 [Arch 官网](https://www.archlinux.org/download/)上下载 archlinux 安装 iso 格式镜像
（文件名类似于 archlinux-2015.11.01-dual.iso），当然也可以从国内镜像网站下载。

```
wget http://mirrors.ustc.edu.cn/archlinux/iso/2018.07.01/archlinux-2018.07.01-x86_64.iso
```

校验 iso 文件：

```
md5sum archlinux-2018.07.01-x86_64.iso
```

如果校验 md5 一致，可将 iso 镜像文件写入 usb 安装盘（设备名称为 /dev/sdx）中：

```
dd if=/path/to/archlinux.iso of=/dev/sdx bs=4M && sync
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
* `/dev/sda3`: / linux root 分区，10G

格式化分区：

```
mkfs.vfat -F32 /dev/sda1
mkswap /dev/sda2
swapon /dev/sda2
mkfs.ext4 /dev/sda3
```


## 挂载分区

```
mount /dev/sda3 /mnt    # 安装系统 root 分区
mkdir -p /mnt/boot /mnt/var /mnt/tmp /mnt/home
mount /dev/sda1 /mnt/boot
```

## 连接网络

iwctl 进入交互界面，用 iwctl 命令连接网络

```
device list
station wlan0 scan
station wlan0 get-networks
station wlan0 connect SSID
```

## 安装基础系统

编辑 `/etc/pacman.d/mirrorlist`，将要使用的源复制到最前面，然后刷新：

```
pacman -Syy
```

安装 `base` 和 `base-devel`：

```
pacstrap /mnt base base-devel linux linux-firmware
```


## 生成 fstab

```
genfstab -U -p /mnt >> /mnt/etc/fstab
cat /mnt/etc/fstab
```


## 切换到新安装系统

```
arch-chroot /mnt
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

其中字体文件可以到 `/usr/share/kbd/consolefonts/` 中查找。


## 设置时区

```
ln -s /usr/share/zoneinfo/Asia/Shanghai /etc/localtime
```


## 设置时间

```
timedatectl set-time "yyyy-MM-dd hh:mm:ss"
hwclock -systohc
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
useradd -m -G wheel user
passwd user
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
options        root=UUID=xxxxxxxxxxxxx rw
```

`options root` 项对应系统 root 分区，其中 `UUID` 可以通过查看文件 `/etc/fstab` 获取，也可以用以下命令获得：

```
ls -l /dev/disk/by-uuid
或者
lsblk -f
```

编辑 `/boot/loader/loader.conf`：

```
timeout 1
default arch
```


## 设置网络

### 有线


```
pacman -S networkmanager
systemctl enable NetworkManager
systemctl start NetworkManager
nmtui
```

图形界面可以安装 `network-manager-applet `。


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

从 archlinux aur 网站下载 aumman PKGBUILD，运行：

```
makepkg
pacman -U 生成的文件，以.xz结尾
```

`makepkg` 有时会报 gpg 验证出错，需要运行：

```
gpg --recv-keys xxxxxxxxID
```
