created: 2011-01-20T13:02:00+08:00
tags: [scheme]

## 字符输出指令

`~~` 指令输出 `~` 字符， params: `n`

```
scheme@(guile-user)> (format #f "~~")
"~"
scheme@(guile-user)> (format #f "~3~")
"~~~"
```

`~%` 指令输出 `newline` 字符，params: `n`

```
scheme@(guile-user)> (format #f "abc~%cde~%")
"abc
cde
"
scheme@(guile-user)> (format #f "abc~3%cde~%")
"abc
 
 
cde
"
```

当不在一行的开始时，`~&` 指令输出一个新行，params: `n`

```
scheme@(guile-user)> (format #f "~&abc~&cde~&")
"abc
cde
"
scheme@(guile-user)> (format #f "~3&abc~3&cde~3&")
"
 
abc
 
 
cde
 
 
"
```

`~_` 指令输出 space 字符，params: `n`

```
scheme@(guile-user)> (format #f "abc~_def")
"abc def"
scheme@(guile-user)> (format #f "abc~3_def")
"abc   def"
```

`~/` 指令输出 tab 字符，params: `n`

```
scheme@(guile-user)> (format #f "abc~/def")
"abc\tdef"
scheme@(guile-user)> (format #f "abc~3/def")
"abc\t\t\tdef"
```

`~|` 指令输出 formfeed 字符，params: `n`

`~!` 指令会在格式化输出最后强制调用 `force-output` 来刷新 dest 的所有缓存。
`~!` 指令可以出现在 fmt 中的任何位置，当 dest 为 `#f` 时，`~!` 指令不做任何事情。

`~c` 指令输出任何相应参数字符，params: `charnum`。

```
scheme@(guile-user)> (format #t "~c" #\a)
a
scheme@(guile-user)> (format #t "~@c" #\a)
#\a
scheme@(guile-user)> (format #t "~:c" #\newline)
^J
scheme@(guile-user)> (format #t "~65c")         ; 调用 (integr->char charnum)
A
```
