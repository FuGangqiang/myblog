created: 2011-01-06T17:57:00+08:00
tags: [scheme]

# 第十五贴：脚本

通常，把想要做的任务直接写到一个文件或脚本中，然后像 shell 命令一样执行它们是非常方便的。一些大型程序通常就提供一些脚本形式的接口，这样，用户就可以经常构建自己的脚本或为了适于某种需求修改已有脚本。编写脚本无疑是最常见的编程，对于大多数用户来说，脚本是他们唯一需要做的编程任务。
 
像 Unix 和 Dos <Windows提供的命令行界面> 操作系统就提供了这样的脚本机制，但是这些脚本机制是比较简单、基础的它们只是一串或一批 shell 命令文件，它使用户执行相同或相似的任务时可以省去再次输入那些命令序列。这些脚本语言只提供了像循环、条件等少量编程要素，只适用于一些小任务，但是当脚本文件变得更大时或者要求更高时，用户这时就需要一种成熟的语言。而那些拥有足够操作系统接口的 scheme 语言会使脚本编程变得更简单和易于维护。
 
本小节将介绍如何用 guile scheme 编写脚本。
 
## Hello world! again
 
hello.scm 文件：

```
#!/usr/local/bin/guile -s
!#
 
(display "Hello World!")
(newline)
```

其中, 第二行的 !# 不能省略，
运行脚本文件前需要修改文件属性使其可执行：

```
$ chmod +x hello.scm
```

运行文件：

```
$ ./hello.scm
Hello World!
```

如果需要命令行参数，guile scheme 提供了 command-line 过程，
下面是计算一个数的阶乘的 fact.scm 文件：

```
#!/usr/local/bin/guile -s
!#
 
(define (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1)))))
 
(display (fact (string->number (cadr (command-line)))))
(newline)
```

运行：

```
$ ./fact.scm 5
120
```

如果要像 C 语言那样有一个 main 函数，fact.scm 可以写为：

```
#!/usr/local/bin/guile \
-e main -s
!#
 
(define (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1)))))
 
(define (main args)
  (display (fact (string->number (cadr args)))))
```

运行：

```
$ ./fact.scm 5
120
```
