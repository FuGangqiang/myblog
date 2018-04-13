created: 2018-04-13T12:07:57+08:00
tags: [ops, vagrant]


[Vagrant][] 让我们可以通过 ruby 脚本来快速、可重复的在本地搭建虚拟机测试集群，
与 ansible/chef 等组合使用更是方便，是本地统一开发测试的极佳运维工具。

[Vagrant]: https://www.vagrantup.com/

本文讲解如何制作最初始的 vagrant box。


## 安装软件

* 安装 virtualbox
* 安装 vagrant


## 制作虚拟机

* 下载 debian iso 镜像
* 利用下载的镜像在 virtualbox 安装一个名为 `vmname` 的 linux 操作系统


## debian 虚拟机配置

* 在虚拟机中创建 `vagrant` 用户和用户目录，密码为 `vagrant`
* 在虚拟机中添加 `vagrant` 用户公共密钥
    ```
    curl https://raw.githubusercontent.com/hashicorp/vagrant/master/keys/vagrant.pub > .ssh/authorized_keys
    ```
* 基础包安装 `apt install ...`
    - sudo
    - gcc
    - make
    - perl
    - openssh-client
    - openssh-server
    - linux-headers-amd64
* 安装 virtualbox guest additions（有可能需要重启）
    - 选择虚拟机菜单 `Devices` -> `Insert Guest Additons CD image...`
    - 挂载 cdrom `mount /dev/cdrom /mnt`
    - 进入 cdrom 目录 `cd /mnt`
    - 安装 virtualbox guest additions `./VBoxLinuxAdditions.run`
* 将 `vagrant` 加入 `sudo` 用户组中，visudo 修改 `%sudo ALL=(ALL:ALL) NOPASSWD:ALL`


## 导出 box

* 在本机中执行 `vagrant package --base vmname /path/to/debian.box`


## 导入 box

`vagrant box add debian /path/to/debian.box`


## 创建 box 虚拟机

* 创建 vagrant 目录 `mkdir debian && cd debian`
* 初始化运行环境, `vagrant init debian`
* 创建虚拟机 `vagrant up`
