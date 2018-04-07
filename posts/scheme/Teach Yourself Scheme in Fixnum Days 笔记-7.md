created: 2010-12-18T00:31:00+08:00
tags: [scheme]

# 第七贴：输入输出

scheme 提供了一些输入输出过程，可以调用它们读取输入端口，写入输出端口，而这些端口<port>可以与控制台<console>、文件<file>或者字符串<string>相关联。
 
## Reading<读取>
 
scheme 读取<reader>过程有一个可选输入端口参数，默认是当前输入端口<通常是控制台>。
 
读取可以是基于字符、行或者是符号表达式。在每一次读取过程中，端口的状态都会更新，以使下一次读取时从已经读过的后面开始。如果一个端口已经读取到结尾，读取过程就返回 `end-of-file` 或 `eof-object` 对象，这个对象可以用 `eof-object?` 函数来判断。
`read-char` 过程读取指定端口下一个字符;
`read-line` 过程读取指定端口的下一行，返回一个字符串<换行符会自动去掉>;
`read` 过程读取指定端口的下一个符号表达式。

## Writing<写入>
 
scheme 写入过程以一个被写入对象和一个可选输出端口为参数，输出端口默认是当前输出端口<通常为控制台>。
`write-char` 过程把一个字符<不带 `#\`>写入到指定端口；
`write` 过程把一个符号表达式以一种 machine-readable 的形式写入指定端口，比如，一个字符串会被双引号括着，而字符会带有 `#\`；
`display` 过程把一个符号表达式以一种 human-readable 的形式写入指定端口，比如，一个字符串不会被双引号括着，而一个字符不会带有 `#\`。

## File ports<文件端口>
 
scheme　输入（输出）过程以标准输入（输出）为默认输入（输出）端口，所以当从标准输入（输出）读取（写入）时，不用指定端口，但是如果你想显式的给出端口，可以调用无参数的 `current-input-port` 和 `current-out-port` 过程：

```
scheme@(guile-user)> (display 9)
9scheme@(guile-user)> (display 9 (current-output-port))
9scheme@(guile-user)>
```

* `open-input-file` 以一个文件名为参数，返回与这个文件相关联的输入端口，当所给文件不存在时会报错；
* `open-output-file` 以一个文件名为参数，返回与这个文件相关联的输出端口，当所给文件已存在时会报错。

当对一个端口完成所有操作时，你应当用 `close-input-port` 和 `close-output-port` 来关闭它。

下面例子中，假设当前文件夹下 hello.txt 文件只含有 `hello` 一个词：

```
scheme@(guile-user)> (define i (open-input-file "hello.txt"))
scheme@(guile-user)> (read-char i)
#\h
scheme@(guile-user)> (define j (read i))
scheme@(guile-user)> j
ello
scheme@(guile-user)> (read-char i)
#\newline
scheme@(guile-user)> (read-char i)
#<eof>
```

下面例子，假设当前文件夹下没有 greeting.txt 文件：

```
scheme@(guile-user)> (define o (open-output-file "greeting.txt"))
scheme@(guile-user)> (display "hello" o)
scheme@(guile-user)> (write-char #\space o)
scheme@(guile-user)> (display 'world o)
scheme@(guile-user)> (newline o)
scheme@(guile-user)> (close-output-port o)
```

此时，在当前文件夹下会有一个 greeting.txt 文件，文件内容为：`hello world`

### Automatic opening and closing of file ports<自动开关文件端口>
 
scheme 中的 `call-with-input-file` 和 `call-with-output-file` 过程会为你自动打开一个端口，
并且当你不再用它时会自动关闭它。
 
`call-with-input-file` 过程以一个文件名和一个过程为参数，而参数过程是以一个端口为参数，
而 `call-with-input-file` 过程的作用就是把与一个文件名相关的端口传给其过程参数，
并且当过程参数返回时确保该端口被关闭：

```
scheme@(guile-user)> (call-with-input-file "hello.txt"
...                     (lambda (i)
...                        (let* ((a (read-char i))
...                               (b (read-char i))
...                               (c (read-char i)))
...                           (list a b c))))
(#\h #\e #\l)
```

`call-with-output-file` 过程与 `call-with-input-file` 过程相似，但是其所服务的对象是输出文件。

## String ports<字符串端口>
 
`open-input-string` 过程使一个字符串与一个端口相关联，而那些读取过程对该端口的操作就是读出字符串中的内容：

```
scheme@(guile-user)> (define i (open-input-string "hello world"))
scheme@(guile-user)> (read-char i)
#\h
scheme@(guile-user)> (read i)
ello
scheme@(guile-user)> (read i)
world
scheme@(guile-user)> (read i)
#<eof>
```

## Loading file<加载文件>
 
load 过程会在当前模块中顺序执行当前文件夹下给定文件名中所有 scheme 语句。
