created: 2011-01-20T13:07:00+08:00
tags: [scheme]

## 特定对象输出指令

* `~a`: display 输出
* `~s`: write 输出
* `~y`: 结构化输出

`~a` 指令格式化输出像 display 过程一样，其 parmas: `minwidth,padinc,minpad,padchar`。

```
scheme@(guile-user)> (format #f "~a" "foo")
"foo"
scheme@(guile-user)> (format #f "~a" car)
"#<procedure car (_)>"
scheme@(guile-user)> (format #f "~:a" car)              ; :a 像字符串那样格式化输出一个没有外部表示(external representation)的对象
"\"#<procedure car (_)>\""
scheme@(guile-user)> (format #f "~5a" 'abc)             ; 指定最大宽度，默认在其右边填充空格
"abc  "
scheme@(guile-user)> (format #f "~5,,,'-@a" 'abc)       ; 指定填充字符，@ 使其在左边插入填充字符
"--abc"
;; 指定最小填充宽度，每次填充宽度的步进
;; 当指定宽度不足时，则其填充宽度为：minpad+n*padinc
;; n 默认为 0，当 n=0 还不够输出格式化对象时，则令 n 加 1，直至满足格式化对象宽度为止
scheme@(guile-user)> (format #f "~5,1,4a" 'abc)         
"abc    "
```

`~s` 指令格式化输出像 write 一样，其 parmas: `minwidth,padinc,minpad,padchar`，类似与 `~a` 的 params。

```
scheme@(guile-user)> (format #f "~s" "foo")
"\"foo\""
scheme@(guile-user)> (format #f "~s" car)
"#<procedure car (_)>"
scheme@(guile-user)> (format #f "~:s" car)
"\"#<procedure car (_)>\""
scheme@(guile-user)> (format #f "~5s" 'abc)
"abc  "
scheme@(guile-user)> (format #f "~5,,,'-@s" 'abc)
"--abc"
scheme@(guile-user)> (format #f "~5,1,4s" 'abc)
"abc    "
```

`~y`: 指令使用 pretty-print 过程格式化输出对象，其 params: `width`；
`~@y`: 指令使用 truncated-print 过程格式化输出对象；
`~:@y`: 像 `~@y` 一样，只是其 params 指定的 `width` 被解释为输出端口的最大列(column)。
