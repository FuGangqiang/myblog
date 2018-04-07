created: 2010-12-11T23:11:00+08:00
tags: [scheme]

# 第四贴：条件语句

像其他语言一样, scheme 也提供了条件语句. 其中, 最基本的语句为 `if`:

```
(if test-expression
    then-branch
    else-branch)
```

如果 test-expression 计算结果为真<除 `#f` 值外>, 执行 then-branch, 否则执行 else-branch.

```
guile> (define p 80)
guile> (if (> p 70)
...        'safe
...        'unsafe)
safe
guile> (if (< p 90)
...        'low-pressure)  ; no else-branch
low-pressure
```

scheme 提供的其他条件语句都可以用 if 语句写出的宏来定义出来.

## cond

`cond` 语句为嵌套的 `if` 语句提供了一种更方便的形式:

```
(if (char<? c #\c)
    1
    (if (char=? c #\c)
    0
    1))
```

可用 `cond` 语句更简单表示为:

```
(cond ((char<? c #\c) -1)
      ((char=? c #\c) 0)
      (else 1))
```

## case

`case` 又是 `cond` 的一种特殊情况的简写语句:

```
guile> (case c
...       ((#\a) 1)
...       ((#\b) 2)
...       ((#\c) 3)
...       (else 4))
3
```

## and 和 or

scheme 也提供了 `and` 和 `or` 语句, 同大多数语言一样, 它们都具有短路特性:

```
guile> (and 1 2)
2
guile> (and #f 1)
#f
guile> (or 1 2)
1
guile> (or #f 1)
```
