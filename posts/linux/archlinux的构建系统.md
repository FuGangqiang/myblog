date: 2015-06-06 19:19:00
tags: linux, archlinux, package


我们知道，Linux 有许多发行版，
常见的有 Redhad、Dedian、Suse 等，
还有许多小众的发行版，比如 Archlinux、Gentoo 等，
有人统计过世界上有数百种不同的 Linux 发行版。

尽管如此众多，它们都是基于 Linux 内核，并在其上包装一些开源软件，
那为什么会出现如此众多的发行版呢？
这就在于它们如何包装构建、安装和管理那些成百上千的开源软件及其依赖，
也就是它们的构建系统，
比如 Redhad 的 yum、Dedian 的 apt、Suse 的 yast、Archlinux 的 pacman、Gentoo 的 emerge。
其中本文只介绍 Archlinux 的构建系统。


## 一切从最开始说起

在介绍构建系统之前，我们先看看在 Linux 下最经典也是最通用的源码编译安装程序的方式：

1. 到开源网站下载相应的源码包
2. 解压源码包
3. 初始化编译环境及配置
4. 构建二进制程序
5. 安装二进制程序

我们以 `nginx` 为例来介绍这一安装过程，
首先我们先到 [nginx 官方网站](http://nginx.org/en/download.html)找到并下载自己需要的 nginx 安装包：

```
wget http://nginx.org/download/nginx-1.8.0.tar.gz
```

解压源码包：

```
tar -xvzf nginx-1.8.0.tar.gz
```

初始化编译环境：

```
cd nginx-1.8.0
./configure
```

构建二进制程序：

```
make
```

安装二进制程序：

```
sudo make install
```

这样就安装好了 nginx 程序。

在安装过程中，有同学会碰到依赖出错提示，
也就是说 nginx 使用了其他软件库，这些库有：pcre 库、zlib 库、openssl 库，
所以在编译安装 nginx 之前，我们需要先把 pcre、zlib 和 openssl 三个库安装到自己的系统里，
然后编译安装 nginx。

但是问题来了，pcre 库、zlib 库和 openssl 库有可能还会依赖其他软件库，
那么我们就要循环向上把所有涉及的依赖库安装到我们的系统中，
当然有可能有些依赖库已经在我们的系统中了，就不须再次安装了。

如果我们不想再用 nginx 了，想把 nginx 彻底的从系统里卸载掉，
那我们只需把所有与 nginx 有关的文件删掉即可。
但是那些 nginx 的依赖库呢？是否也须把它们也删除了？
这里就要小心了，因为我们系统中有可能还有其他程序依赖这些库，
如果我们删除了这些依赖库，会导致那些依赖这些库的程序无法运行。
如果我们不删除这些依赖库，随着安装卸载各种程序的积累，系统中就会遗留一些不再被用到的依赖库文件了，就是我们通常说的垃圾文件。

不仅如此，我们怎么找到那些与 nginx 有关的所有的文件呢？
这就要从我们运行 `make install` 命令时用到的 makefile 文件里找了，
当然一般 makefile 文件也有 `uninstall` 命令供你选择。

从上面可以看出，在 Linux 系统上从源码编译安装卸载程序会是一件很繁琐及容易出错的事情，
所以人们想到了各种方法来简化安装及卸载软件的繁琐过程，
于是就出现了各种不同的构建系统。


## Pacman 命令

在 Archlinux 系统里，如果我们需要安装 nginx 程序，当然可以像上面那样安装，
但是更好也是最简单的方法就是利用 Archlinux 中的 `pacman` 命令：

```
pacman -S nginx
```

这样，就把 nginx 及其依赖（pcre 库、zlib 库和 openssl 库）安装到你的系统中了，
你就可以直接调用 `nginx` 命令运行了。

如果我们想彻底的把 nginx 命令卸载呢？很简单：

```
pacman -Rsn nginx
```

这样，就把 nginx 相关文件及其依赖彻底的删除了，
但是如果系统中还有其他程序依赖 pcre 库、zlib 库或 openssl 库时，
pacman 就不会删除这个库，这样就保证我们不会碰到删除依赖库的同时其它依赖于这个库的程序无法运行的情况。

pacman 命令是 `package manager` 的简写，是 Archlinux 系统中的包管理器，简单使用如下：

1. 更新系统
    * `pacman -Syu` 更新整个 Archlinux 系统
    * `pacman -Sy` 将本地的包数据库与远程的仓库进行了同步
    * `pacman -Su` 根据本地包数据库更新系统
    * `pacman -Syu –ignore 包名` 在更新系统时不升级指定包
2. 安装包
    * `pacman -S 包名` 安装指定包，也可以同时安装多个包，只需以空格分隔包名即可
    * `pacman -Sy 包名` 与上面命令不同的是，该命令将先同步包数据库然后再安装指定包名
    * `pacman -Sf 包名` 强制安装指定包
    * `pacman -Sv 包名` 在显示一些操作信息后执行安装
    * `pacman -U  包名.pkg.tar.gz` 安装本地包
3. 删除包
    * `pacman -R 包名` 只删除指定包
    * `pacman -Rs 包名` 在删除指定包的同时，也将删除其依赖
    * `pacman -Rd 包名` 在删除指定包时不检查其依赖
    * `pacman -Rn 包名`　删除软件包及其配置文件
    * `pacman -Rsn 包名`　删除软件包及其依赖和配置文件
4. 查询同步(sync)包数据库
    * `pacman -Ss 关键字` 在包数据库中查询含有关键字的包
    * `pacman -Si 包名`　查询包数据库中有关包的详细信息
5. 查询本地(local)包数据库
    * `pacman -Q` 列出系统中所有已安装包的包名
    * `pacman -Qs 关键字`　在包数据库中查询含有关键字的包
    * `pacman -Qi 包名` 在包数据库中查询有关包的详细信息
    * `pacman -Ql 包名` 在包数据库中查询并列出该包所含文件
    * `pacman -Qo /path/file` 查询某文件属于何包
    * `pacman -Qdt` 查询系统中的孤立包
    * `pacman -Qet` 查询系统中的不被因依赖而安装的包
6. 其他用法
    * `pacman -Sw 包名` 只下载包，不安装
    * `pacman -Sc` 清理未安装的包文件（Pacman 下载的包文件缓存于 /var/cache/pacman/pkg/ 目录）
    * `pacman -Scc` 清理所有的缓存文件
    * `pacman -Rs $(pacman -Qdtq)` 递归删除系统中存在的孤立包


## pacman 原理

上面我们看到了 pacman 有很多方便的功能，下面就让我们来看一下 pacman 命令是怎么做到这些的。

我们手动简单模拟一下 `pacman -S nginx` 是怎么工作的：

```
wget http://mirrors.163.com/archlinux/extra/os/x86_64/nginx-1.8.0-1-x86_64.pkg.tar.xz
pacman -U nginx-1.8.0-1-x86_64.pkg.tar.xz
```

我们先从网上下载最新的 `nginx` 的 archlinux 包，然后安装它，这就是 pacman 安装程序的大致过程。

那这个过程是怎么被 pacman 命令隐藏的呢？我们来看一下 pacman 命令的配置文件 `/etc/pacman.conf` 里被注释掉的两行：

```
#DBPath      = /var/lib/pacman/
#CacheDir    = /var/cache/pacman/pkg/
```

从这里我们可以大致看到：`DBPath` 是用来存储 archlinux 的包数据库的，而 `CacheDir` 是用来存放下载的 archlinux 包的。

在运行 `pacman -S nginx` 时，
pacman首先会从 `/var/lib/pacman/sync/` 文件夹里的几个数据库中搜索到 `nginx` 包对应的所有依赖包，
并递归搜索相应依赖包的所有依赖包，统计出安装 `nginx` 必须要安装的所有包，
再从 `/var/lib/pacman/local/` 文件夹里查询这些包是否已经被安装，如果已被安装，则忽略安装此包及其依赖，
接着 pacman 就会从 `/etc/pacman.d/mirrorlist` 文件里选择一个包下载地址，
把这些包下载到 `/var/cache/pacman/pkg/` 文件夹里，安装包到系统里，
同时更新 `/var/lib/pacman/local/` 文件夹，用来注册这些新安装的包，
到此安装过程结束。

## package

读到这里，你也许会问，我们安装的 `nginx-1.8.0-1-x86_64.pkg.tar.xz` 包到底是什么东东？

从文件后缀我们可以知道它是一个压缩包，解压一下，我们看看里面是什么：

```
$ tar xvJf nginx-1.8.0-1-x86_64.pkg.tar.xz
$ tree .
.
├── etc
│   ├── logrotate.d
│   │   └── nginx
│   └── nginx
│       ├── fastcgi.conf
│       ├── fastcgi_params
│       ├── koi-utf
│       ├── koi-win
│       ├── mime.types
│       ├── nginx.conf
│       ├── scgi_params
│       ├── uwsgi_params
│       └── win-utf
├── usr
│   ├── bin
│   │   └── nginx
│   ├── lib
│   │   └── systemd
│   │       └── system
│   │           └── nginx.service
│   └── share
│       ├── licenses
│       │   └── nginx
│       │       └── LICENSE
│       ├── man
│       │   └── man8
│       │       └── nginx.8.gz
│       ├── nginx
│       │   └── html
│       │       ├── 50x.html
│       │       └── index.html
│       └── vim
│           └── vimfiles
│               ├── ftdetect
│               │   └── nginx.vim
│               ├── indent
│               │   └── nginx.vim
│               └── syntax
│                   └── nginx.vim
└── var
    ├── lib
    │   └── nginx
    │       └── proxy
    └── log
        └── nginx
```

从目录树里我们可以看到，这就是安装 `nginx` 的所有文件，
其中有二进制文件和配置文件(`usr/`、`etc/` 和 `var/` 目录)，
并且路径与操作系统中的目录树相对应。


## makepkg

`nginx-1.8.0-1-x86_64.pkg.tar.xz` 这个包是由 archlinux 的维护者们事先打包构建的，
把它放到 `mirrorlist` 服务器上，供 pacman 命令下载。

那这个包又是怎么构建的呢？
其实这个包构建的步骤与我们在本文第一节讲过的大致相同，
下载，解压，编译，安装。

但是 archlinux 也额外做了一些其他事情，比如下载安装依赖包等等，
为了简化这一繁琐过程，archlinux 有了 `makepkg` 命令。

`makepkg` 命令是通过一个名为 `PKGBUILD` 脚本下载、解压、编译并打包成 archlinux 包来完成上面所有事情的。

首先我们编写一个 `PKGBUILD` 脚本用来构建 `nginx-1.8.0-1-x86_64.pkg.tar.xz`：

```
mkdir build
touch PKGBUILD
```

编写 `PKGBUILD` 内容如下：

```bash
pkgname=nginx
pkgver=1.8.0
pkgrel=1
pkgdesc='Lightweight HTTP server and IMAP/POP3 proxy server'
arch=('i686' 'x86_64')
url='http://nginx.org'
license=('custom')
depends=('pcre' 'zlib' 'openssl' 'geoip')
makedepends=('hardening-wrapper')
source=($url/download/nginx-$pkgver.tar.gz)
md5sums=('3ca4a37931e9fa301964b8ce889da8cb')

build() {
  cd $pkgname-$pkgver

  ./configure \
    --prefix=/etc/nginx \
    --conf-path=/etc/nginx/nginx.conf \
    --sbin-path=/usr/bin/nginx \
    --pid-path=/run/nginx.pid \
    --lock-path=/run/lock/nginx.lock \
    --user=http \
    --group=http \
    --http-log-path=/var/log/nginx/access.log \
    --error-log-path=stderr \
    --http-client-body-temp-path=/var/lib/nginx/client-body \
    --http-proxy-temp-path=/var/lib/nginx/proxy \
    --http-fastcgi-temp-path=/var/lib/nginx/fastcgi \
    --http-scgi-temp-path=/var/lib/nginx/scgi \
    --http-uwsgi-temp-path=/var/lib/nginx/uwsgi \
    --with-imap \
    --with-imap_ssl_module \
    --with-ipv6 \
    --with-pcre-jit \
    --with-file-aio \
    --with-http_dav_module \
    --with-http_gunzip_module \
    --with-http_gzip_static_module \
    --with-http_realip_module \
    --with-http_spdy_module \
    --with-http_ssl_module \
    --with-http_stub_status_module \
    --with-http_addition_module \
    --with-http_degradation_module \
    --with-http_flv_module \
    --with-http_mp4_module \
    --with-http_secure_link_module \
    --with-http_sub_module \
    --with-http_geoip_module

  make
}

package() {
  cd $pkgname-$pkgver
  make DESTDIR="$pkgdir" install

  install -Dm644 contrib/vim/ftdetect/nginx.vim \
    "$pkgdir"/usr/share/vim/vimfiles/ftdetect/nginx.vim
  install -Dm644 contrib/vim/syntax/nginx.vim \
    "$pkgdir"/usr/share/vim/vimfiles/syntax/nginx.vim
  install -Dm644 contrib/vim/indent/nginx.vim \
    "$pkgdir"/usr/share/vim/vimfiles/indent/nginx.vim

  sed -e 's|\<user\s\+\w\+;|user html;|g' \
    -e '44s|html|/usr/share/nginx/html|' \
    -e '54s|html|/usr/share/nginx/html|' \
    -i "$pkgdir"/etc/nginx/nginx.conf

  rm "$pkgdir"/etc/nginx/*.default

  install -d "$pkgdir"/var/lib/nginx
  install -dm700 "$pkgdir"/var/lib/nginx/proxy

  chmod 750 "$pkgdir"/var/log/nginx
  chown http:log "$pkgdir"/var/log/nginx

  install -d "$pkgdir"/usr/share/nginx
  mv "$pkgdir"/etc/nginx/html/ "$pkgdir"/usr/share/nginx

  install -Dm644 LICENSE "$pkgdir"/usr/share/licenses/$pkgname/LICENSE

  rmdir "$pkgdir"/run

  install -d "$pkgdir"/usr/share/man/man8/
  gzip -9c man/nginx.8 > "$pkgdir"/usr/share/man/man8/nginx.8.gz
}
```

在 `build/` 目录下运行 `makepkg` 命令，
就可以看到了所有与本文第一节类似的步骤，
然后就自动生成了 `nginx-1.8.0-1-x86_64.pkg.tar.xz` 包。

当然，此时你可以把新生成的包安装到系统中：

```
pacman -U nginx-1.8.0-1-x86_64.pkg.tar.xz
```

## PKGBUILD

从上面可以看到，为了构建一个 archlinux 包，
我们只需编写 `PKGBUILD` 脚本，
然后运行 `makepkg` 命令，
其通过 `PKGBUILD` 脚本中参数和指令来自动化下载、解压、编译和打包全过程。

`PKGBUILD` 非常好写，通过 shell 语法定义一些 `makepkg` 命令需要的参数和指令。

我们来看看上一小节中 `PKGBUILD` 中定义的参数：

* pkgname：包名
* pkgver：包版本
* pkgrel：包的 release number
* pkgdesc：包的简要概述
* arch：包编译的体系结构
* url：包官方网站 url
* depends：包的依赖包
* makedepends：编译这个包时需要的依赖包
* source：包源码的下载地址
* md5sums：上面 source 参数对应项的 md5sum，用来检测下载包的正确性
* build：编译指令
* package：打包指令

更多参数请参考 [archlinux wiki](https://wiki.archlinux.org/index.php/PKGBUILD)。


## AUR(Archlinux User-community Repository)

开源世界是一个分享的世界，archlinux 通过 AUR 来存放来自世界上所有用户贡献的 `PKGBUILD` 脚本，
进而使大家方便的通过 archlinux 安装软件。

那么如何贡献 AUR 呢？很简单，只需下面步骤：

* 编写 `PKGBUILD` 相关文件
* 打包 `PKGBUILD` 相关文件
* 上传打包文件到 http://aur.archlinux.org 即可

相对应的命令如下：

```
cd pkgname
touch PKGBUILD
cd ..
tar -zcvf pkgname.tar.gz pkgname
```

那么我们如何利用AUR呢？

你可以：

* 在AUR网站上搜索你想要的包
* 下载相应的 `pkgname.tar.gz`
* 解压包
* 进入目录运行 `makepkg` 命令
* 利用 `pacman -U` 来安装相应包

有些同学可能会问：那 AUR 和 `pacman -S pkgname` 又有什么关系呢？

在 archlinux 系统里，所有的包都是从 `PKGBUILD` 开始构建的，你通过 `pacman` 得到的二进制包也是从 `PKGBUILD` 编译而成的。
在 `/etc/pacman.conf` 有很多 `Repository` 设置，其中的 `[core]`、`[extra]` 是由 archlinux 的核心成员维护的，
这些软件库里的软件由于特别重要，每个人都要用，所以 archlinux 的开发人员把二进制包提前做好，你就可以通过 `pacman` 取得了 。

而 `/etc/pacman.conf` 里的 `[community]` 里的包就是先由 AUR 社区用户贡献，通过下载用户投票，当投票数超过一定数量后，
由 archlinux 维护者检查这个包的 `PKGBUILD` 相关文件是否安全，如果安全，就会把这个包移入 `[community]` 源里。


## ABS(Archlinux Build System)

ABS 默认没有被安装到 archlinux 里，你可以通过 `pacman -S abs` 命令安装它。

那么 ABS 又是什么呢？
ABS 就是把所有包的 `PKGBUILD` 相关文件全部下载到你的系统里(`/var/abs/`)。

你可以到 `/var/abs/` 里面复制相应包的 `PKGBUILD` 相关文件，
运行 `makepkg` 和 `pacman -U` 命令来打包安装它了，
当然，你也可以按照你的需要修改相应的 `PKGBUILD` 文件，继而自定义安装包。
