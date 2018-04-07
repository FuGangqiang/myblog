created: 2010-11-25T13:38:00+08:00
tags: [scheme]


以下为阴阳谜题的 scheme 代码：

```
(let* ((yin ((lambda (foo) (display "@") foo)
             (call/cc (lambda (bar) bar))))
       (yang ((lambda (foo) (display "*") foo)
              (call/cc (lambda (bar) bar)))))
  (yin yang))
```

以上程序运行的结果为：

```
@*@**@***@****@*****@******
...
```

之所以会出现如此结果（循环渐增的打印星号），都是 scheme 中的 continuation 在作怪！
以下让我们来分析以下这个程序吧!

其中：

```
(call/cc (lambda (bar) bar))
```

此表达式返回当前的 continuation

```
(lambda (foo) (display "@") foo)
```

此表达式先是打印 "@" 字符，然后把 foo 参数作为函数的返回值返回

```
((lambda (foo) (display "@" foo))
     (call/cc (lambda (bar) bar)))
```

此表达式先是打印 "@" 字符，然后把当前的 continuation 作为函数返回值返回

### 环境1

在 `let*` 语句中，首先对 `yin`、`yang` 变量依次赋值，
`yin`、`yang` 都被赋为一个 continuation，
两个 continuation 假设分别为 `@CC` 和 `*CC`：

```
yin=@CC       同时打印 @
yang=*CC      同时打印 *
```

运行 `(yin yang)`，即为 `(@CC *CC)`。

### 环境2

此时，由于 `(@CC *CC)` 表达式调用 `@CC` continuation 把 `*CC` 作为返回值返回
从而 `yin`、`yang` 被重新赋值（两者仍都是一个 continuation）:

```
yin=*CC         同时打印 @
yang=**CC       同时打印 *
```

之所以 `yang` 等于 `**CC` 而不是 `*C`，是因为 `yang` 被赋值前，`yin` 已被赋值为 `*C`，
其不同于 `环境1` 中的 `yin`（`环境1` 中的 `yin` 被赋值为 `@CC`）。

运行 `(yin yang)`，即为 `(*CC **CC)`。

### 环境3

此时，由于 `(*CC **CC)` 表达式调用 `*CC` continuation 把 `**CC` 作为返回值返回，
在此要注意 `*CC` 所在的环境（`yin` 已被赋值为 `@CC`，`yang`被赋值为 `call/cc` 函数的返回值，在此即为 `**CC`）
从而 `yang` 被重新赋值：

```
yin=@CC
yang=**CC      同时打印 *
```

运行 `(yin yang)`，即为 `(@CC **CC)`。


### 环境4

此时，由于 `(@CC **CC)` 表达式调用 `@CC` continuation 把 `**CC` 作为返回值返回，
从而 `yin`、`yang` 被重新赋值：

```
yin=**CC        同时打印 @
yang=***CC      同时打印 *
```

之所以 yang 等于 `***CC`，是因为 `yang` 被赋值前，`yin` 已被赋值为`**C`，
其不同于 `环境1`/`环境2` 中的 `yin`（`环境1` 中的被赋值为 `@CC`，`环境2` 中被赋值为 `*CC`）

运行 `(yin yang)`，即为 `(**CC ***CC)`


环境...,往复循环不止