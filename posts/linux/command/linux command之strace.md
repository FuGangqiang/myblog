created: 2015-05-14T22:16:14+08:00
tags: [linux, CLI]


strace 可以用来监控一个进程与操作系统内核间的交互（也就是系统调用）和接收的信号，
因此是一个很好用的诊断、调试工具，

一般有两种情况我们会用到 strace 命令：

* 不知道程序源代码时，用 strace 监控程序运行时所有的系统调用和接受的信号，以便分析程序的功能
* 当一个进程发生了异常陷入僵死状态时，从输出的日志上并没有找到任何线索，可以用 strace 来查看进程处于何种状态


## 监控 ls 命令

这里用 ls 命令为引子来简要说明 strace 命令的输出。

我们知道，ls 命令是用来列出文件夹里的文件列表的，那它背后究竟调用了那些系统调用呢？
这里我们可以用 strace 来监控一下：

```
~$ strace ls
execve("/usr/bin/ls", ["ls"], [/* 39 vars */]) = 0
brk(0)                                  = 0x925000
access("/etc/ld.so.preload", R_OK)      = -1 ENOENT (No such file or directory)
open("/etc/ld.so.cache", O_RDONLY|O_CLOEXEC) = 3
fstat(3, {st_mode=S_IFREG|0644, st_size=155200, ...}) = 0
mmap(NULL, 155200, PROT_READ, MAP_PRIVATE, 3, 0) = 0x7f256ff79000
close(3)                                = 0
open("/usr/lib/libcap.so.2", O_RDONLY|O_CLOEXEC) = 3
read(3, "\177ELF\2\1\1\0\0\0\0\0\0\0\0\0\3\0>\0\1\0\0\0\240\25\0\0\0\0\0\0"..., 832) = 832
fstat(3, {st_mode=S_IFREG|0644, st_size=17320, ...}) = 0
mmap(NULL, 4096, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0) = 0x7f256ff78000
mmap(NULL, 2112568, PROT_READ|PROT_EXEC, MAP_PRIVATE|MAP_DENYWRITE, 3, 0) = 0x7f256fb7a000
mprotect(0x7f256fb7e000, 2093056, PROT_NONE) = 0
mmap(0x7f256fd7d000, 4096, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_FIXED|MAP_DENYWRITE, 3, 0x3000) = 0x7f256fd7d000
close(3)                                = 0
open("/usr/lib/libc.so.6", O_RDONLY|O_CLOEXEC) = 3
read(3, "\177ELF\2\1\1\3\0\0\0\0\0\0\0\0\3\0>\0\1\0\0\0000\7\2\0\0\0\0\0"..., 832) = 832
fstat(3, {st_mode=S_IFREG|0755, st_size=1991416, ...}) = 0
mmap(NULL, 3815984, PROT_READ|PROT_EXEC, MAP_PRIVATE|MAP_DENYWRITE, 3, 0) = 0x7f256f7d6000
mprotect(0x7f256f971000, 2093056, PROT_NONE) = 0
mmap(0x7f256fb70000, 24576, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_FIXED|MAP_DENYWRITE, 3, 0x19a000) = 0x7f256fb70000
mmap(0x7f256fb76000, 14896, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_FIXED|MAP_ANONYMOUS, -1, 0) = 0x7f256fb76000
close(3)                                = 0
open("/usr/lib/libattr.so.1", O_RDONLY|O_CLOEXEC) = 3
read(3, "\177ELF\2\1\1\0\0\0\0\0\0\0\0\0\3\0>\0\1\0\0\0`\24\0\0\0\0\0\0"..., 832) = 832
fstat(3, {st_mode=S_IFREG|0755, st_size=18736, ...}) = 0
mmap(NULL, 2113912, PROT_READ|PROT_EXEC, MAP_PRIVATE|MAP_DENYWRITE, 3, 0) = 0x7f256f5d1000
mprotect(0x7f256f5d5000, 2093056, PROT_NONE) = 0
mmap(0x7f256f7d4000, 8192, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_FIXED|MAP_DENYWRITE, 3, 0x3000) = 0x7f256f7d4000
close(3)                                = 0
mmap(NULL, 4096, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0) = 0x7f256ff77000
mmap(NULL, 4096, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0) = 0x7f256ff76000
mmap(NULL, 4096, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0) = 0x7f256ff75000
arch_prctl(ARCH_SET_FS, 0x7f256ff76700) = 0
mprotect(0x7f256fb70000, 16384, PROT_READ) = 0
mprotect(0x7f256f7d4000, 4096, PROT_READ) = 0
mprotect(0x61c000, 4096, PROT_READ)     = 0
mprotect(0x7f256ff9f000, 4096, PROT_READ) = 0
munmap(0x7f256ff79000, 155200)          = 0
brk(0)                                  = 0x925000
brk(0x946000)                           = 0x946000
open("/usr/lib/locale/locale-archive", O_RDONLY|O_CLOEXEC) = 3
fstat(3, {st_mode=S_IFREG|0644, st_size=6870064, ...}) = 0
mmap(NULL, 6870064, PROT_READ, MAP_PRIVATE, 3, 0) = 0x7f256ef43000
close(3)                                = 0
ioctl(1, TCGETS, {B38400 opost isig icanon echo ...}) = 0
ioctl(1, TIOCGWINSZ, {ws_row=48, ws_col=151, ws_xpixel=1359, ws_ypixel=720}) = 0
stat(".", {st_mode=S_IFDIR|0755, st_size=4096, ...}) = 0
open(".", O_RDONLY|O_NONBLOCK|O_DIRECTORY|O_CLOEXEC) = 3
fstat(3, {st_mode=S_IFDIR|0755, st_size=4096, ...}) = 0
getdents(3, /* 3 entries */, 32768)     = 80
getdents(3, /* 0 entries */, 32768)     = 0
close(3)                                = 0
fstat(1, {st_mode=S_IFCHR|0620, st_rdev=makedev(136, 0), ...}) = 0
mmap(NULL, 4096, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0) = 0x7f256ff9e000
write(1, "hello\n", 6hello
)                  = 6
close(1)                                = 0
munmap(0x7f256ff9e000, 4096)            = 0
close(2)                                = 0
exit_group(0)                           = ?
+++ exited with 0 +++
```

输出的大多数行是系统调用，基本格式都一样：

```
execve("/usr/bin/ls", ["ls"], [/* 39 vars */]) = 0
```

首先是系统调用名 `execve`，接着括号里是系统调用的参数列表，最后等号后是系统调用的返回值。
可以看出我们执行 ls 命令用的是 `/usr/bin/ls` 这个程序，其参数只有 `ls`，也就是命令本身，还继承了父进程里的 39 个环境变量。

```
brk(0)                                  = 0x925000
```

`brk` 系统调用参数是 0，用来获取进程数据段末尾地址。

接着是大量的 `open, fstat, mmap` 映射动态库来初始化 c 的运行时，而最后几行就是我们想要 ls 做的事情：

```
open(".", O_RDONLY|O_NONBLOCK|O_DIRECTORY|O_CLOEXEC) = 3
fstat(3, {st_mode=S_IFDIR|0755, st_size=4096, ...}) = 0
getdents(3, /* 3 entries */, 32768)     = 80
getdents(3, /* 0 entries */, 32768)     = 0
close(3)                                = 0
fstat(1, {st_mode=S_IFCHR|0620, st_rdev=makedev(136, 0), ...}) = 0
mmap(NULL, 4096, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0) = 0x7f256ff9e000
write(1, "hello\n", 6)                  = 6
close(1)                                = 0
```

打开当前目录，获取文件夹的 `directory entry`，然后将结果输出到标准输出。


## 输出详解

strace 输出的系统调用一般如下：

```
open("/dev/null", O_RDONLY) = 3
```

但当系统调用发生错误时如下：

```
open("/foo/bar", O_RDONLY) = -1 ENOENT (No such file or directory)
```

strace 会帮我们把错误的名称和说明一并列出。

进程接收到信号时，一般会这样打印：

```
sigsuspend([] <unfinished ...>
--- SIGINT (Interrupt) ---
+++ killed by SIGINT +++
```

在利用多线程时，有可能存在一个系统调用被另一个线程的系统调用打断，然后再次被恢复调用，输出是这样的：

```
[pid 28772] select(4, [3], NULL, NULL, NULL <unfinished ...>
[pid 28779] clock_gettime(CLOCK_REALTIME, {1130322148, 939977000}) = 0
[pid 28772] <... select resumed> )      = 1 (in [3])
```

当一个可重启的系统调用被信号打断后并重启时，输出是这样的：

```
read(0, 0x7ffff72cf5cf, 1)              = ? ERESTARTSYS (To be restarted)
--- SIGALRM (Alarm clock) @ 0 (0) ---
rt_sigreturn(0xe)                       = 0
read(0, ""..., 1)                       = 0
```

## 命令选项

可以用 `-e` 参数来过滤某个系统调用：

```
$ strace -e open ls
open("/etc/ld.so.cache", O_RDONLY|O_CLOEXEC) = 3
open("/usr/lib/libcap.so.2", O_RDONLY|O_CLOEXEC) = 3
open("/usr/lib/libc.so.6", O_RDONLY|O_CLOEXEC) = 3
open("/usr/lib/libattr.so.1", O_RDONLY|O_CLOEXEC) = 3
open("/usr/lib/locale/locale-archive", O_RDONLY|O_CLOEXEC) = 3
open(".", O_RDONLY|O_NONBLOCK|O_DIRECTORY|O_CLOEXEC) = 3
+++ exited with 0 +++
```

当然，我们可以列出多个系统调用：

```
$ strace -e trace=open,fstat ls
open("/etc/ld.so.cache", O_RDONLY|O_CLOEXEC) = 3
fstat(3, {st_mode=S_IFREG|0644, st_size=155200, ...}) = 0
open("/usr/lib/libcap.so.2", O_RDONLY|O_CLOEXEC) = 3
fstat(3, {st_mode=S_IFREG|0644, st_size=17320, ...}) = 0
open("/usr/lib/libc.so.6", O_RDONLY|O_CLOEXEC) = 3
fstat(3, {st_mode=S_IFREG|0755, st_size=1991416, ...}) = 0
open("/usr/lib/libattr.so.1", O_RDONLY|O_CLOEXEC) = 3
fstat(3, {st_mode=S_IFREG|0755, st_size=18736, ...}) = 0
open("/usr/lib/locale/locale-archive", O_RDONLY|O_CLOEXEC) = 3
fstat(3, {st_mode=S_IFREG|0644, st_size=6870064, ...}) = 0
open(".", O_RDONLY|O_NONBLOCK|O_DIRECTORY|O_CLOEXEC) = 3
fstat(3, {st_mode=S_IFDIR|0755, st_size=4096, ...}) = 0
fstat(1, {st_mode=S_IFCHR|0620, st_rdev=makedev(136, 0), ...}) = 0
+++ exited with 0 +++

```

可以用 `-o` 可以使 strace 结果输出至某个文件：

```
strace -o open.txt ls
```

可以用 `-p` 来让 strace 追踪某个正在运行的进程：

```
strace -p 1234
```

其中 `1234` 就是进程的进程号。

可以用 `-t` 来输出系统调用的绝对时间，用 `-r` 来输出系统调用的相对时间：

```
$ strace -t -e open ls
22:02:05 open("/etc/ld.so.cache", O_RDONLY|O_CLOEXEC) = 3
22:02:05 open("/usr/lib/libcap.so.2", O_RDONLY|O_CLOEXEC) = 3
22:02:05 open("/usr/lib/libc.so.6", O_RDONLY|O_CLOEXEC) = 3
22:02:05 open("/usr/lib/libattr.so.1", O_RDONLY|O_CLOEXEC) = 3
22:02:05 open("/usr/lib/locale/locale-archive", O_RDONLY|O_CLOEXEC) = 3
22:02:05 open(".", O_RDONLY|O_NONBLOCK|O_DIRECTORY|O_CLOEXEC) = 3
22:02:05 +++ exited with 0 +++
$ strace -r -e open ls
     0.000000 open("/etc/ld.so.cache", O_RDONLY|O_CLOEXEC) = 3
     0.000288 open("/usr/lib/libcap.so.2", O_RDONLY|O_CLOEXEC) = 3
     0.000335 open("/usr/lib/libc.so.6", O_RDONLY|O_CLOEXEC) = 3
     0.000330 open("/usr/lib/libattr.so.1", O_RDONLY|O_CLOEXEC) = 3
     0.000955 open("/usr/lib/locale/locale-archive", O_RDONLY|O_CLOEXEC) = 3
     0.000399 open(".", O_RDONLY|O_NONBLOCK|O_DIRECTORY|O_CLOEXEC) = 3
     0.000610 +++ exited with 0 +++
```

可以用 `-c` 来统计设计到的系统调用的统计信息：

```
$ strace -c ls
hello
% time     seconds  usecs/call     calls    errors syscall
------ ----------- ----------- --------- --------- ----------------
  0.00    0.000000           0         3           read
  0.00    0.000000           0         1           write
  0.00    0.000000           0         6           open
  0.00    0.000000           0         8           close
  0.00    0.000000           0         1           stat
  0.00    0.000000           0         7           fstat
  0.00    0.000000           0        14           mmap
  0.00    0.000000           0         7           mprotect
  0.00    0.000000           0         2           munmap
  0.00    0.000000           0         3           brk
  0.00    0.000000           0         2           ioctl
  0.00    0.000000           0         1         1 access
  0.00    0.000000           0         1           execve
  0.00    0.000000           0         2           getdents
  0.00    0.000000           0         1           arch_prctl
------ ----------- ----------- --------- --------- ----------------
100.00    0.000000                    59         1 total
```

更多选项请看 man 文档。
