created: 2011-01-20T13:10:00+08:00
tags: [scheme]

## 控制指令

`~newline`: Continuation line，`~` 后紧跟回车，无 params。
这个指令可以把一个长的 format 字符串打断为数行，增加可读性。

```
scheme@(guile-user)> (format #f "abc~           ; ~newline 忽略回车和紧跟其后的空格
                                 ~d def~
                                 ~d" 1 2)
"abc1 def2"
scheme@(guile-user)> (format #f "abc~:          ; ~:newline 忽略回车但保留紧跟其后的空格
                                 ~d def~:
                                 ~d" 1 2)
"abc                                 1 def                                 2"
scheme@(guile-user)> (format #f "abc~@          ; ~@newline 保留回车但忽略紧跟其后的空格
                                 ~d def~@
                                 ~d" 1 2)
"abc
1 def
2"
```

`~?`,`~k`: Sub-format 指令，无 params。

```
scheme@(guile-user)> (format #f "~?" "~d ~d" '(1 2))            ; ~? 指令用相应的参数作为指令，且将下一个参数<一个列表>作为指令所对应参数
"1 2"
scheme@(guile-user)> (format #f "~k" "~d ~d" '(1 2))            ; ~k 与 ~? 同
"1 2"
scheme@(guile-user)> (format #f "~@? ~s" "~d ~d" 1 2 "foo")     ; ~@? 指令用相应的参数作为指令，且将接着几个参数作为指令所对应参数
"1 2 \"foo\""
```

`~(~)`: 大小写转换指令<Case conversion>，无 params。

* `~(...~)` 将会使其间的字符串转换为小写；
* `~:@(...~)` 将会使其间的字符串转换为大写；

现在 format 过程只支持这两种转换，以后会推出增强版本，使其像 Common Lisp 一样也支持 `~:(...~)` 和 `~@(...~)` 使每个单词的第一个字母大写

```
scheme@(guile-user)> (format #f "~(HEllo~)")    ; 小写形式
"hello"
scheme@(guile-user)> (format #f "~:@(HEllo~)")  ; 大写形式
"HELLO"
```

`~[~;~]`: 条件指令，其 params: `selector`。
一个条件指令块由` ~[、~;、~]` 分割成数个子句，由 `~[` 所对应的参数<integer>来选择对应的子句，第一个子句所对应的整数为 0。

```
scheme@(guile-user)> (format #f "~[peach~;banana~;mango~]" 1)           ; 第一个子句为 peach，第二个为 banana，第三个为 mango，~[ 所对应的参数为 1，因此选择第二个子句输出，注意：0 对应第一个子句
"banana"
scheme@(guile-user)> (format #f "~2[peach~;banana~;mango~]")            ; 当提供 selector 时，~[ 不需要参数，selector 与之对应
"mango"
scheme@(guile-user)> (format #f "~[peach~;banana~;mango~]" 9)           ; 当所个参数超出子句数时，不输出任何字符
""
scheme@(guile-user)> (format #f "~[peach~;banana~;mango~:;fruit~]" 9)   ; 当提供 ~:; 时，其后子句为所给参数超出子句数时默认值
"fruit"
scheme@(guile-user)> (format #f "~:[false~;not false~]" #f)             ; ~:[ 只需两个子句，当其所对应参数值为 #f 选择第一个子句，否则选择第二个语句
"false"
scheme@(guile-user)> (format #f "~:[false~;not false~]" #t)
"not false"
scheme@(guile-user)> (format #f "~:[false~;not false~]" 'abc)
"not false"
scheme@(guile-user)> (format #f "~@[temperature=~d~]" 27)               ; ~@[ 只需一个子句，当其所对应参数为 #t 时，选择其子句，并对其应用该参数，否则不应用该参数
"temperature=27"
```

`~{~}`: 列表迭代指令<Iteration>，其 params: `maxreps`。
`~{` 和 `~}` 之间的部分会被迭代，默认对参数列表中的每一个元素进行迭代。

```
scheme@(guile-user)> (format #f "~{~d~}" '(1 2 3))      ; 每次迭代消耗一个列表元素
"123"
scheme@(guile-user)> (format #f "~{~a=~d ~}" '("x" 1 "y" 2 "z" 3)) ; 每次迭代消耗两个列表元素
"x=1 y=2 z=3 "
```

`~:{` 其对应的参数为一个列表的列表，每一个子列表将会被迭代

```
scheme@(guile-user)> (format #f "~:{~dx~d ~}" '((1 2) (3 4) (5 6)))
"1x2 3x4 5x6 "
```

`~@{` 对其以后参数进行迭代：

```
scheme@(guile-user)> (format #f "~@{~d~}" 1 2 3)
"123"
scheme@(guile-user)> (format #f "~@{~a=~d ~}" "x" 1 "y" 2 "z" 3)
"x=1 y=2 z=3 "
```

`~:@` 也对其以后的参数进行迭代，但每个参数须为列表：

```
scheme@(guile-user)> (format #f "~:@{~dx~d ~}" '(1 2) '(3 4) '(5 6))
"1x2 3x4 5x6 "
```

`~^`: 跳出<Escape>指令，其 params：`val1,val2,val3`。
如果没有对应的参数就退出格式化过程：

```
scheme@(guile-user)> (format #f "~d~^ ~d" 1)
"1"
scheme@(guile-user)> (format #f "~d ~d" 1)
ERROR:missing argument(s)
scheme@(guile-user)> (format #f "~d~^ ~d" 1 2)
"1 2"
```

当 `~^` 指令处于 `~{ ~}` 指令中时，如果没有多余的参数，`~^` 指令就跳出当前迭代<可处于嵌套循环中>：

```
scheme@(guile-user)> (format #f "~{~d~^/~} go" '(1 2 3))
"1/2/3 go"
scheme@(guile-user)> (format #f "~{~d/~} go" '(1 2 3))
"1/2/3/ go"
```

当 `~^` 指令处于 `~?` 指令相对应的参数中时，只是退出 sub-format：

```
scheme@(guile-user)> (format #f "~? items" "~d~^ ~d" '(1))
"1 items"
scheme@(guile-user)> (format #f "~? items" "~d~^ ~d" '(1 2))
"1 2 items"
```

`~*`: 跳转指令，其 params: `n`。

```
scheme@(guile-user)> (format #f "~d ~2*~d" 1 2 3 4)             ; ~2* 表示向后跳 2 个参数
"1 4"
scheme@(guile-user)> (format #f "~d ~d ~2:*~d ~d" 1 2 3 4)      ; ~2:* 表示向前跳 2 个参数
"1 2 1 2"
scheme@(guile-user)> (format #f "~d ~2@* ~d" 1 2 3 4)           ; ~2@* 表示跳到第 2 个参数，n=0 表示第一个参数
"1  3"
```
