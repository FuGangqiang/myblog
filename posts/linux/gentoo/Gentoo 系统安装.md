created: 2021-11-01T19:11:23+08:00
tags: [linux, gentoo]


前天开始在家用笔记本上安装 Gentoo Linux 系统，
并没有想像的复杂，步骤得当，很容易安装成功，
现将安装经历总结如下：


## 安装前注意事项

- 本文安装方法不适用于虚拟机安装，虚拟机安装需要配置不同的内核参数，应该还有其他注意事项。
- 如果是新手，需提前阅读、理解官方 Handbook 安装步骤，与网上一些同学安装方法进行比对更佳。
- 如果不熟悉内核配置，建议用 `genkernel` 工具来编译、安装内核。
- 安装前，建议先阅读一下 portage 的使用及原理，使用 portage 相关命令遇到报错时会很有用。
- 在安装过程中，如果安装失败，可以通过启动 U 盘，重新 chroot 进去，修复系统

gentoo 安装文档参考：

- [Handbook](https://wiki.gentoo.org/wiki/Handbook:Main_Page)
- [Quick Installation Checklist](https://wiki.gentoo.org/wiki/Quick_Installation_Checklist)


## 搭建环境

- amd64 架构
- gpt 分区
- gentoo
- systemd-boot 引导加载
- systemd 初始化进程


## 安装基本步骤

- 制作 Linux 启动 U 盘
- BIOS 启动选项设置
- 安装准备
- 磁盘分区、格式化
- 下载、解压 Gentoo Stage3 压缩文件
- 配置修改
- chroot
- 更新系统
- 编译 Linux 内核
- 建立 fstab 文件
- 配置系统
- 安装引导程序
- 重启
- 安装桌面系统
- 清理系统


## 制作 Linux 启动 U 盘

不建议用 Gentoo 官方提供的启动盘镜像，
因为安装系统时，需要手动输入许多命令，
这些命令可以通过网页文档轻松复制得到，
使用有图形化可引导的 Linux 启动 U 盘，
可以节省很多时间，
考察一番后，选择了 manjaro-xfce iso 镜像文件来制作启动盘，
下载网址为 [https://manjaro.org/download/](https://manjaro.org/download/)。

下载完毕后，校验下载镜像文件是否有误：

```
sha1sum manjaro-xfce-21.1.4-210927-linux513.iso
```

检测 hash 字符串，确保与官方网站一致。

dd 命令制作 USB 启动盘：

```
dd if=./manjaro-xfce-21.1.4-210927-linux513.iso of=/dev/sdc bs=4M status=progress oflag=sync
```

`dd` 命令是 Linux 平台下的，如果需要在 Windows 下制作启动盘需使用 `Rufus` 工具。


## BIOS 启动选项设置

 将 U 盘插入笔记本，开机，当刚出现开机画面时，按 `F2`进入 BIOS 系统，这里有些系统有可能是其他键。

 将 Boot Mode 设为 UEFI Mode，关闭 Secure Boot。

 调整优先从 U 盘启动。

 保存设置并退出。

 进入开机画面，此时显示的就是启动 U 盘系统的加载画面。


## 安装准备

进入 U 盘的 Manjaro Linux 系统后，打开终端和浏览器，
如果使用 WiFi 联网，需要选择设置 WiFi 网络。

在浏览器中可以打开 Gentoo Handbook， 一边看文档，一边复制命令在终端执行。

在终端中切换到 root 用户环境：

```
su
```


## 磁盘分区、格式化

我笔记本里面只有一个 238GiB 固态硬盘，分区方案如下：

分区        |  挂载路径     |  类型  |  大小
-----------|--------------|--------|-----
boot 分区   |  /boot       | fat32  | 512MiB
swap 分区   |  linux swap  | swap   | 4GiB
root 分区   |  /           | ext4   | 100GiB
home 分区   |  /home       | ext4   | 100GiB
backup 分区 |  不挂载，备用  | ext4   | 34GiB

注意这里单位是 GiB，而不是 GB。

### 查看分区列表

磁盘分区官方文档用的是 `fdisk`，本文用的是 `parted` 命令。

列出系统所有磁盘分区情况：

```
parted -l
```

我这里列出两个磁盘：

- /dev/sda       30GiB       // USB 启动盘
- /dev/nvme0n1   256GiB      // 笔记本固态硬盘

注意这里列表里面的单位是 GB，而不是 GiB。

### 进入分区命令

准备将 gentoo 系统安装在笔记本的固态硬盘上面，所以对 `/dev/nvme0n1` 磁盘进行分区：

```
parted /dev/nvme0n1
```

进入 parted 命令模式，下面所有磁盘分区操作都是在 parted 命令模式下输入。


### 修改成 GPT 分区表

```
(parted) mklabel GPT
```

出现警告「硬盘数据将会全部丢失」，回答 Yes


### 创建分区

```
(parted) mkpart boot fat32 1MiB 512MiB
(parted) mkpart swap linux-swap 512MiB 4GiB
(parted) mkpart root ext4 4GiB 104GiB
(parted) mkpart home ext4 104GiB 204GiB
(parted) mkpart backup ext4 204GiB 100%
(parted) set 1 esp on
(parted) quit
```

注意这里单位是 GiB，而不是 GB。

### 格式化分区

```
mkfs.fat -F 32 /dev/nvme0n1p1
mkswap /dev/nvme0n1p2
mkfs.ext4 /dev/nvme0n1p3
mkfs.ext4 /dev/nvme0n1p4
mkfs.ext4 /dev/nvme0n1p5
```

## 下载、解压 Gentoo Stage3 压缩文件

### 挂载主分区

```
swapon /dev/nvme0n1p2
mkdir -p /mnt/gentoo
mount /dev/nvme0n1p3 /mnt/gentoo
```

### 下载 Gentoo Stage3 压缩文件

可以直接从官网下载，为了加快下载速度，我是用的清华源：

```
cd /mnt/gentoo
wget https://www.mirrorservice.org/sites/distfiles.gentoo.org//releases/amd64/autobuilds/20211024T170536Z/stage3-amd64-systemd-20211024T170536Z.tar.xz
```

验证下载的 gentoo stage3 包：

```
sha512sum stage3-amd64-systemd-20211024T170536Z.tar.xz
# 相对应 hash 地址：https://mirror.isoc.org.il/pub/gentoo/releases/amd64/autobuilds/20211024T170536Z/stage3-amd64-systemd-20211024T170536Z.tar.xz.DIGESTS
# 检测 hash 是否与官网提供的一致
```

### 解压 Gentoo Stage3 压缩文件

检测如果一致，说明下载正确，可以进行解压操作：

```
tar xpvf stage3-amd64-systemd-20211024T170536Z.tar.xz --xattrs-include='*.*' --numeric-owner
```


## 配置修改


### 修改 `portage/make.conf` 文件

```
nano -w /mnt/gentoo/etc/portage/make.conf
```

修改内容如下：

```
COMMON_FLAGS="-march=native -O2 -pipe"
CFLAGS="${COMMON_FLAGS}"
CXXFLAGS="${COMMON_FLAGS}"

DISTDIR="/var/cache/distfiles"
PKGDIR="/var/cache/binpkgs"

MAKEOPTS="-j8"
ACCEPT_LICENSE="*"
GENTOO_MIRRORS="https://mirrors.tuna.tsinghua.edu.cn/gentoo"
```

其中：

- `MAKEOPTS` 里面的数字与 CPU 核数对应，加快编译速度，
- `GENTOO_MIRRORS` 使用的清华源，加快下载速度

### 修改 `portage/repos.conf` 文件

```
mkdir --parents /mnt/gentoo/etc/portage/repos.conf
cp /mnt/gentoo/usr/share/portage/config/repos.conf /mnt/gentoo/etc/portage/repos.conf/gentoo.conf
nano -w /mnt/gentoo/etc/portage/repos.conf/gentoo.conf
```

修改内容如下：

```
[DEFAULT]
main-repo = gentoo

[gentoo]
location = /var/db/repos/gentoo
sync-type = rsync
sync-uri = rsync://rsync.gentoo.org/gentoo-portage
auto-sync = yes
sync-rsync-verify-jobs = 1
sync-rsync-verify-metamanifest = yes
sync-rsync-verify-max-age = 24
sync-openpgp-key-path = /usr/share/openpgp-keys/gentoo-release.asc
sync-openpgp-key-refresh-retry-count = 40
sync-openpgp-key-refresh-retry-overall-timeout = 1200
sync-openpgp-key-refresh-retry-delay-exp-base = 2
sync-openpgp-key-refresh-retry-delay-max = 60
sync-openpgp-key-refresh-retry-delay-mult = 4
```

### 修改 `resolv.conf` 文件

```
cp --dereference /etc/resolv.conf /mnt/gentoo/etc/
```


## chroot

```
mount --types proc /proc /mnt/gentoo/proc
mount --rbind /sys /mnt/gentoo/sys
mount --make-rslave /mnt/gentoo/sys
mount --rbind /dev /mnt/gentoo/dev
mount --make-rslave /mnt/gentoo/dev
mount --bind /run /mnt/gentoo/run
mount --make-slave /mnt/gentoo/run
chroot /mnt/gentoo /bin/bash
source /etc/profile
export PS1="(chroot) ${PS1}"
```

挂载 boot 分区：

```
mount /dev/nvme0n1p1 /boot
```


## 更新系统

### 同步 ebuild 库

```
emerge-webrsync
emerge --sync
```

### 选择系统 profile

```
eselect profile list
eselect profile set NUMBER
```

这里根据自己的需要，选择了 systemd 的那个 profile


### systemd-boot 预设置

```
echo "sys-apps/systemd gnuefi" >> /etc/portage/package.use/systemdboot
```

### 升级系统

```
emerge -a --update --deep --newuse @world
```


## 编译 Linux 内核

```
emerge --ask sys-kernel/linux-firmware
emerge --ask sys-kernel/gentoo-sources
eselect kernel list
eselect kernel set NUMBER
```

- 这里的 `NUMBER` 为选择的内核编号

### 手动编译安装

```
cd /usr/src/linux
make menuconfig
make -j5 && make modules_install && make install

# 安装 initramfs
emerge --ask sys-kernel/genkernel
genkernel --install --kernel-config=/path/to/used/kernel.config initramfs
```

### 工具辅助编译安装

建议新手用 `genkernel` 进行编译、安装内核：

```
genkernel --install --kernel-config=/path/to/used/kernel.config all
```

查看生成的内核文件和 initramfs:

```
ls /boot/vmlinuz* /boot/initramfs*
```

## 建立 fstab 文件

查看各分区的 UUID 和 PARTUUID:

```
blkid
```

创建 fstab 文件：

```
nano -w /etc/fstab
```

根据磁盘分区情况设置文件内容如下：

```
UUID=XXX-XXX    /          ext4       rw,relatime	0 1
UUID=XXX-XXX    /boot      vfat       defaults     0 0
UUID=XXX-XXX    none       swap       sw           0 0
UUID=XXX-XXX    /home      ext4       defaults     0 2
```


## 配置系统

### 用户配置

```
passwd   root
useradd -m -G wheel fu
passwd  fu
```

### 语言本地化配置

```
echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
echo "zh_CN.UTF-8 UTF-8" >> /etc/locale.gen
echo "zh_CN.GB18030 GB18030" >> /etc/locale.gen
echo "zh_CN.GBK GBK" >> /etc/locale.gen
echo "zh_CN GB2312" >> /etc/locale.gen
locale-gen
localectl set-locale en_US.utf8
```

### 时区配置

```
timedatectl set-timezone Asia/Shanghai
```

### 主机名配置

```
hostnamectl set-hostname Gentoo
```

### 网络配置

使用 systemd-networkd 的话需要确保 `/etc/resolv.conf` 是一个软链接，链接到 `/run/systemd/network/resolv.conf`。

#### 有线网络

创建网络配置文件：

```
nano -w /etc/systemd/network/20-wired.network
```

内容如下：

```
[Match]
Name=enp1s0

[Network]
DHCP=yes
```

其中：

- `enp1s0` 是无线网卡名，可以通过 `ip link` 查看得到

开机启动联网：

```
systemctl enable systemd-networkd.service
```

#### 无线网络

安装、配置 wpa_passphrase：

```
emerge --ask net-wireless/wpa_supplicant
wpa_passphrase MyNetwork SuperSecretPassphrase > /etc/wpa_supplicant/wpa_supplicant-wlp2s0.conf
systemctl enable wpa_supplicant@wlp2s0.conf
```

其中：

- `wlp2s0` 是无线网卡名，可以通过 `ip link` 查看得到
- `MyNetwork` 是无线网络名
- `SuperSecretPassphrase` 是无限网络密码

创建网络配置文件：

```
nano -w /etc/systemd/network/25-wireless.network
```

内容如下：
```
[Match]
Name=wlp2s0

[Network]
DHCP=yes
```

开机启动联网：
```
systemctl enable systemd-networkd.service
```

### NetworkManager 联网

最简单的联网方法就是用 NetworkManager，但是它比较重。

修改 `/etc/portage/make.conf` 的 `USE` 选项：

```
USE="${USE} networkmanager"
```

安装 NetworkManager：
```
emerge --ask --deep --new-use @world
emerge --ask net-misc/networkmanager
```

修改普通用户组：
```
usermod -a -G plugdev fu
```

开机启动 NetworkManager：
```
systemctl enable NetworkManager
```

### 更新环境

```
env-update && source /etc/profile
```


## 安装引导程序

安装 UEFI 启动管理器：

```
bootctl install
```

添加系统启动项：

```
nano -w /boot/loader/entries/gentoo.conf
```

根据 `/boot` 内的 `vmlinuz` 和 `initramfs` 的文件名，设置内容如下：
```
title Gentoo Linux
linux /vmlinuz-XXX
initrd /initramfs-XXX
options root=PARTUUID=XXXXXX rw init=/lib/systemd/systemd
```

其中：

- `PARTUUID` 是通过 `blkid` 命令查看分区得到的

调整启动管理器配置：

```
nano -w /boot/loader/loader.conf
```

内容如下：
```
default gentoo
timeout 1
```

## 重启

基本系统安装完毕，就可以作为服务器进行配置了。

```
exit
reboot
```

## 安装桌面系统

```
emerge --ask x11-base/xorg-server
emerge --ask x11-wm/i3
emerge --ask x11-terms/st

echo "exec i3" > ~/.xinitrc
```

之后就用 `startx` 命令就可以进入 i3 图形窗口管理了。


## 清理系统

```
rm /stage3-amd64-systemd-20211024T170536Z.tar.xz

emerge --depclean
emerge --ask app-portage/gentoolkit
eclean --deep distfiles
eclean --deep packages
```
