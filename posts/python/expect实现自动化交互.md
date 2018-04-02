created: 2015-04-04T17:02:00+08:00
tags: [python, 自动化]


在Linux下我们大多通过shell脚本来自动完成一些命令，节约了大量时间，
但是当你执行一些需要交互的命令时，
shell就有一点捉襟见肘了，
最好的解决方案就是使用expect命令。


## expect是什么？

expect是用于提供自动交互的工具，
其原理是在后台建立一个虚拟终端来运行程序，通过控制它的输入输出流来自动化完成交互的功能。
它被广泛应用于交互式操作和自动化测试的场景之中，
尤其适用于需要对多台服务器执行相同操作的环境中，
可以大幅度提高系统管理人员的工作效率。

比如：
你要用ssh登录服务器的时候，
命令行下提示需要输入密码登录，
登录后就可以在服务器上执行命令了，
如果用shell脚本来直接调用ssh命令，
shell不会为你自动输入密码，
它会一直停在那里等你输入，
当你键入密码登录后，就进入了和服务器交互的界面，
在这里shell也不会自动为你交互执行命令，
那如何来实现ssh自动化登录，然后自动执行一些命令呢？
答案就是expect！


## expect示例

expect是建立在tcl语言之上的，
因此学习expect也就要学习tcl语言，
其用法如下：

```bash
#!/usr/bin/expect

spawn ssh user@host

expect password:
send "your password\n"
interact
```

这个示例展示了expect最重要的三个命令：spawn、expect和send命令。


## pexpect示例

上面是一个很简单的例子，
在很复杂的交互的情况下，
expect可以通过tcl语言定义一些变量，函数，使用它的控制结构，
但是作为一个python语言使用者，自然想通过python来完成这些自动化功能，
pexpect模块为此而生：

```python
#!/usr/bin/python

import pexpect

c = pexpect.spawn('ssh user@host')
c.expect('password:')
c.sendline('your password')
c.interact()
```
