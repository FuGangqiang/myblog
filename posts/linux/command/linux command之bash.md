created: 2014-03-15T13:07:04+08:00
tags: [linux, CLI]


## shell

一般来讲，shell(外壳)就是用户和 UNIX 操作系统间的用户界面，用户通过它输入一些指令，进而将指令翻译为操作系统提供服务的调用，产生的结果再通过它展现给用户。

常见的有两类：

* CLI: command line interfaces，命令提示符界面
* GUI: graphical user interfaces，图形用户界面


## bash

bash 是一种基于字符(character-based)的一种 CLI shell，Bourne-Again SHell 的缩写。

bash 完全兼容 sh，并且从 ksh 和 csh 中引入了一些有用的功能，Linux 操作系统首选 Shell。


## 示例

让我们来开一下它是怎么工作的，示例如下：

```
sort -n phonelist > phonelist.sorted
```

以上命令的作用是：将 phonelist 文件中所有行按照数字排序后的结果写到文件 phonelist.sorted。
那么，bash 是如何处理这条命令的呢？

1. 把命令拆分成以子串：`sort`、`-n`、`phonelist`、`>` 和 `phonelist.sorted`。
2. 判断子串的意义：`sort` 是一个命令，`-n` 和 `phonelist` 是参数，`>` 和 `phonelist.sorted` 是 I/O 指令。
3. 通过 `>` 和 `phonelist.sorted` 设置 I/O（输出重定向为当前文件夹下的 `phonelist.sorted` 文件）。
4. 找到 `sort` 命令，把参数 `-n` 和 `phonelist` 传给它，并执行它。

当然，上面的每一步还可以在细分为许多步骤，这里只是大致简单的看一下 bash 解释命令的方式。


## 命令优先级

bash 有多种命令，通过优先级来解释它们:

1. 别名(aliases)
2. 关键字(keywords): function、if、for 等等
3. 函数(function)
4. 内置命令(built-ins)：cd、type 等等
5. 脚本(scripts)和可执行程序(executable)


## 字符串操作符

bash 有多种字符串操作符：

1. `${var:-word}`: 如果 var 存在且不是 null，返回 var 的值，否则返回 word。
2. `${var:=word}`: 如果 var 存在且不是 null，返回 var 的值，否则先将 var 的值设为 word,然后返回它的值。
3. `${var:?message}`: 如果 var 存在且不是 null，返回返回 var 的值，否则打印出 var: message 且终止程序。
4. `${var:+word}`: 如果 var 存在且不是 null，返回 word，否则返回 null。
5. `${var:offset:length}`：截取 var 字符串

注：
前四种还有另一种形式，就是没有那个`:`，
它们的作用与原来的相似，只是去掉条件中的`且不是 null`，其他都不变。


## 模式匹配

bash 有多种模式匹配：

1. `${var#pattern}`：如果 pattern 匹配(最小匹配) var 的开头，则返回剩余未匹配部分。
2. `${var##pattern}`：如果 pattern 匹配(最大匹配)var 的开头，则返回剩余未匹配部分。
3. `${var%pattern}`：如果 pattern 匹配(最小匹配)var 的尾部，则返回剩余未匹配部分。
4. `${var%%pattern}`：如果 pattern 匹配(最大匹配)var 的尾部，则返回剩余未匹配部分。
5. `${var/pattern/string}`：将 pattern 与 var 匹配(最大匹配)的替换为 string，但只替换一次。
6. `${var//pattern/string}`：将 pattern 与 var 匹配(最大匹配)的替换为 string，但所有匹配都替换。

其中的 `pattern` 可以为形为

* `*(patternlist)`
* `+(patternlist)`
* `?(patternlist)`
* `@(patternlist)`
* `!(patternlist)`

而 `patternlist` 形为：`pat1|pat2|pat3`。

示例：

```
path=/home/cam/book/long.file.pcx
echo ${path##/*/}       #                long.file.pcx
echo ${path#/*/}        #       cam/book/long.file.pcx
echo ${path}            # /home/cam/book/long.file.pcx
echo ${path%.*}         # /home/cam/book/long.file
echo ${path%%.*}        # /home/cam/book/long

echo ${path%.pcx}.jpg   # /home/cam/book/long.file.jpg
```


## 控制结构

* if/else

```
if condition
then
    commands
[elif condition
then
    commands]
[else
    commands]
fi
```

* for

```
for name [in list]
do
    commands that can use $name
done

for (( expr1 ; expr2 ; expr3 ))
do
    commands
done

for i in 1 2 3 4 5
do
   echo $i
done


for i in {1..5}
do
   echo $i
done


for i in {0..10..2}
do
    echo $i
done


for i in $(seq 0 10 2)
do
   echo $i
done


for (( c=1; c<=5; c++ ))
do
   echo $i
done
```

* case

```
case expression in
    pattern1 )
        commands
        ;;
    pattern2 )
        commands
        ;;
    ...
esac
```

* select

```
select name [in list]
do
    commands that can use $name
done
```

* while

```
while condition
do
    commands
done
```

* until

```
until condition
do
    commands
done
```


## 函数

下面两种形式一样：

```
fn1() {
    commands
}

function fn2 {
    commands
}
```
