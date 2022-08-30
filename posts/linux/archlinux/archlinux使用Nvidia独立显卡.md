created: 2022-08-30T17:20:00+08:00
tags: [linux, archlinux]


从事服务器开发，对显卡要求不高，
虽然笔记本自带了 Nvidia 独立显卡，但是一直使用的是 Intel 集成显卡，
最近学习 Blender 3D 软件，开始要求显卡性能，想到了笔记本的 Nvidia 独立显卡，
开始了一番摸索，记录如下。

本文主要是关于如何在 Archlinux 系统里只使用 Nvidia 显卡的方法，
内容大部分来源于 [wiki](https://wiki.archlinux.org/title/NVIDIA_Optimus)，
不过如果只按照这个文档中的步骤，我的笔记本没有成功启动 Nvidia 显卡，
所以也加了一些其他的步骤。


## 安装 nvidia 驱动

```
pacman -S nvidia
```

这里需要根据自己的显卡类型来判断安装哪个包，具体请看[Archlinux wiki NVIDIA 页面](https://wiki.archlinux.org/title/NVIDIA)。


## 删除 intel 驱动

确保系统中的 intel 显卡驱动被删除：

```
pacman -Rsn xf86-video-intel vulkan-intel
```


## 添加 xorg 配置文件

确保系统中没有 `/etc/X11/xorg.conf` 文件，并添加配置文件 `/etc/X11/xorg.conf.d/10-nvidia-drm-outputclass.conf` 如下：

```
Section "OutputClass"
    Identifier "intel"
    MatchDriver "i915"
    Driver "modesetting"
EndSection

Section "OutputClass"
    Identifier "nvidia"
    MatchDriver "nvidia-drm"
    Driver "nvidia"
    Option "AllowEmptyInitialConfiguration"
    Option "PrimaryGPU" "yes"
    ModulePath "/usr/lib/nvidia/xorg"
    ModulePath "/usr/lib/xorg/modules"
EndSection
```


## xinitrc 文件配置

由于系统设置从 startx 进入图形界面，所以需要在 `~/.xinitrc` 文件添加命令如下：

```
xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto
xrandr --dpi 96
```

如果是使用其他的显示管理器(Display manager)，需要参考文档中对应配置修改。


## 修改内核参数，开启 DRM 内核 mode setting

由于系统是 systemd-boot 启动的，所以需要对 `/boot/loader/entries/arch.conf` 文件的 `options` 行添加参数如下：

```
nvidia_drm.modeset=1
```


## 修改 pacman hooks

需要当 nvidia 包升级时自动重新制作 initramfs 镜像文件，添加 `/etc/pacman.d/hooks/nvidia.hook` 文件如下：

```
[Trigger]
Operation=Install
Operation=Upgrade
Operation=Remove
Type=Package
Target=nvidia
Target=linux
# Change the linux part above and in the Exec line if a different kernel is used

[Action]
Description=Update NVIDIA module in initcpio
Depends=mkinitcpio
When=PostTransaction
NeedsTargets
Exec=/bin/sh -c 'while read -r trg; do case $trg in linux) exit 0; esac; done; /usr/bin/mkinitcpio -P'
```


## 重新制作内核 initramfs 镜像文件

```
mkinitcpio -p linux
```


## 重启

进入系统后，可以利用下面命令检查是否已经启用了 Nvidia 独立显卡：

```
glxinfo | grep NVIDIA
```

如果系统中没有 `glxinfo` 命令需要安装 `mesa-utils` 包。


## 其他利用 Nvidia 独立显卡的方法

如果平时利用 intel 显卡，个别应用可以利用 `prime-run` 命令在需要时切换显卡：

```
prime-run blender
```
