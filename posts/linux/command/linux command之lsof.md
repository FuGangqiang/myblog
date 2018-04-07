created: 2015-05-16T21:37:00+08:00
tags: [linux, CLI]


lsof 是 list open files 的简写， 在 Unix-like 系统中经常被用来列出系统中所有被打开的文件，以及由哪些进程打开它们的。

在 Unix-like 中，“一切皆文件”，
包括普通文件、文件夹、设备文件、字符文件、正在被程序执行的文本段索引、库、流和网络文件（Internet socket、NFS 文件、Unix domain socket）。


## 列出打开文件

列出系统中所有打开文件需要 root 权限：

```
# lsof
COMMAND     PID   USER   FD      TYPE             DEVICE SIZE/OFF       NODE NAME
init          1   root  cwd       DIR                8,3     4096          2 /
init          1   root  rtd       DIR                8,3     4096          2 /
init          1   root  txt       REG                8,3   150352     527181 /sbin/init
init          1   root  mem       REG                8,3    65928     654110 /lib64/libnss_files-2.12.so
init          1   root    0u      CHR                1,3      0t0       4021 /dev/null
```

lsof 输出各列信息的意义如下：

* COMMAND：进程的名称
* PID：进程标识符
* USER：进程所有者
* FD：文件描述符，应用程序通过文件描述符识别该文件。如cwd、txt等
    * cwd：应用程序的当前工作目录
    * rtd：根目录
    * txt：文件类型是程序代码，应用程序二进制文件本身或共享库
    * mem：内存映射文件
    * r：部分只读锁
    * w：部分只写锁
    * R：整个文件读锁
    * W：整个文件写锁
    * u：任意大小读写锁
    * U：Unknow type 锁
    * 初始打开每个应用程序时，都具有三个文件描述符，从 0 到 2，分别表示标准输入、输出和错误流。所以大多数应用程序所打开的文件的 FD 都是从 3 开始。
* TYPE：文件类型，如 DIR、REG 等
    * DIR：目录
    * REG：普通文件
    * CHR：字符特殊文件
    * FIFO：先进先出
    * UNIX：unix 域套接字
* DEVICE：指定磁盘的名称
* SIZE：文件的大小
* NODE：索引节点inode（文件在磁盘上的标识）
* NAME：打开文件的确切名称


## 列出某用户所打开文件

使用 `-u` 选项列出指定某个用户打开文件，其后可以是 login name 或 UID：

```
lsof -u apache
```

也可以列出多个用户所打开的文件：

```
lsof -u apache,vim,211
```

使用 `^` 来排除某用户打开的文件：

```
lsof -u apache,vim,^211
```


## 列出某用户组所打开文件

同 `-u` 类似，`-g` 是用来指定用户组：

```
lsof -g 123,^wheel
```


## 列出某进程所打开文件

使用 `-p` 可以列出指定进程所打开的文件：

```
lsof -p 123,456,^789
```


## 列出某命令名所打开文件

使用 `-c` 可以列出某命令名以指定字符串开头的进程所打开的文件：

```
lsof -c abc
```

可以求反：

```
lsof -c ^abc
```

可以组合多个 `-c`：

```
lsof -c abc -c def
```


## 列出相关网络连接

使用 `-i [46][protocol][@hostname|hostaddr][:service|port]` 可以列出系统中所打开的网络链接：

其后如果无参数，默认列出所有网络链接：

```
lsof -i
```

* `[46]` 用来过滤 IPV4 或 IPV6 地址
* `protocol` 用来过滤 TCP 和 UDP 协议的
* `@hostname` 或 `@hostaddr` 用来指定 IP 地址的，IPV4 地址为 `xxx,xxx,xxx,xxx`，IPV6 地址为 `[xx::xxx:]`
* `:service` 或 `:port` 用来过滤端口的

```
-i 6           # IPv6 only
-i TCP:25      # TCP and port 25
-i @1.2.3.4    # Internet IPv4 host address 1.2.3.4
-i @[3ffe:1ebc::1]:1234   # Internet IPv6 host address 3ffe:1ebc::1, port 1234
-i UDP:who                # UDP who service port
-i TCP@lsof.itap:513      # TCP, port 513 and host name lsof.itap
-i tcp@foo:1-10,smtp,99   # TCP, ports 1 through 10, service name smtp, port 99, host name foo
-i tcp@bar:1-smtp         # TCP, ports 1 through smtp, host bar
-i :time                  # either TCP, UDP or UDPLITE time service port
```


## AND 选项

`-a` 做 AND 操作

```
lsof -a -u fu -i tcp:25
```


## 列出打开文件的特定域


`-t` 选项可以很方便的列出 PID：

```
lsof -t
```

当是更通用的是 `-F f`，用来列出指定列：

* a：access: r = read; w = write; u = read/write
* c：command name
* d：device character code
* D：major/minor device number as 0x<hex>
* f：file descriptor (always selected)
* G：file flaGs
* i：inode number
* k：link count
* K：task ID (TID)
* l：lock: r/R = read; w/W = write; u = read/write
* L：login name
* m：marker between repeated output
* n：comment, name, Internet addresses
* o：file offset as 0t<dec> or 0x<hex>
* p：process ID (PID)
* g：process group ID (PGID)
* P：protocol name
* r：raw device number as 0x<hex>
* R：paRent PID
* s：file size
* S：stream module and device names
* t：file type
* T：TCP/TPI info
* u：user ID (UID)
* 0：(zero) use NUL field terminator instead of NL

可以通过 `-F ?` 来查看都有哪些参数。

