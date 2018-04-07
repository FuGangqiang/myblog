created: 2011-01-20T13:16:00+08:00
tags: [scheme]

## 其他指令

`~p`: 名词复数<Plural>后缀表示，无 params。

```
scheme@(guile-user)> (format #f "enter name~p" 1)       ; 参数为 1 时，~p 什么都不输出
"enter name"
scheme@(guile-user)> (format #f "enter name~p" 2)       ; 参数不为 1 时，~p 输出字符 's'
"enter names"
scheme@(guile-user)> (format #f "pupp~@p" 1)            ; 参数为 1 时，~@p 输出字符 'y'
"puppy"
scheme@(guile-user)> (format #f "pupp~@p" 2)            ; 参数不为 1 时，~@p 输出 'ies'
"puppies"
scheme@(guile-user)> (format #f "~d cat~:p" 9)          ; ~:p 与 ~p 类似，只是其利用前一个指令参数<这里为 ~d 所对应的参数>作为其对应的参数
"9 cats"
scheme@(guile-user)> (format #f "~d pupp~:@p" 9)        ; ~:@p 与 ~@p 类似，只是其利用前一个指令参数<这里为 ~d 所对应的参数>作为其对应的参数
"9 puppies"
```

`~t`: 列自增指令<Advance to a column position>，其 params：`colnum,colinc,padchar`。

```
scheme@(guile-user)> (format #f "~tX")                  ; 添加 padchar<默认为空格> 至第 1 列<colnum默认为1，从 0 开始>
" X"
scheme@(guile-user)> (format #f "~3tX")                 ; 添加 padchar 至第 3 列
"   X"
scheme@(guile-user)> (format #f "abcd~2,5,'.tx")        ; 添加 padchar<这里为 .> 至 2+n*5 列，n 默认为 0，当当前列数大于指定列数时，使 n 增 1，直至使 2+n*5 大于当前列数
"abcd...x"
scheme@(guile-user)> (format #f "a~3,5,'*@tx")          ; ~@3t 从当前列开始添加 3 个 padchar，当 colinc 提供时，再添加至符合要求最小的 colinc 的倍数列
"a****x"
```

`~q`: format 过程版本号输出指令

```
scheme@(guile-user)> (format #f "~q")           
"SLIB Common LISP format version 3.0
  (C) copyright 1992-1994 by Dirk Lutzebaeck
  please send bug reports to `lutzeb@cs.tu-berlin.de'
"
```
