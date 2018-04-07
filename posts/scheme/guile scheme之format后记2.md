created: 2011-01-18T13:14:00+08:00
tags: [scheme]

## 数值输出指令

在格式化输出中，数值输出是最常见的形式，其通常分为：整数、浮点数和复数。
 
 
### 整数输出指令

* `~b`: 二进制输出指令
* `~o`: 八进制输出指令
* `~d`: 十进制输出指令
* `~x`: 十六进制输出指令

```
scheme@(guile-user)> (format #f "~b" 16)
"10000"
scheme@(guile-user)> (format #f "~o" -16)
"-20"
scheme@(guile-user)> (format #f "~d" 16)
"16"
scheme@(guile-user)> (format #f "~x" -16)
"-10"
```

如果想要非负整数前有一加号，可以用 `@` 指令选项：

```
scheme@(guile-user)> (format #f "~@b" 16)
"+10000"
scheme@(guile-user)> (format #f "~@o" -16)
"-20"
scheme@(guile-user)> (format #f "~@d" 16)
"+16"
scheme@(guile-user)> (format #f "~@x" -16)
"-10"
```

如果想要控制数值输出的最小宽度及多余宽度填充字符或其他格式，我们可以用 param 可选项，
这些整数指令的 param 可选项格式为： `minwidth,padchar,commachar,commawidth`

```
scheme@(guile-user)> (format #f "~5d" 16)       ;宽度为5
"   16"
scheme@(guile-user)> (format #f "~5,'*d" 16)    ;宽度为5，填充字符为 *
"***16"
scheme@(guile-user)> (format #f "~5,'0d" 16)    ;宽度为5，填充字符为 0
"00016"
scheme@(guile-user)> (format #f "~3,'0d" 1234)  ;宽度为3，超出则忽略之，按实际输出
"1234"
```

如果你想要这样的大数写法：`1,000,000,000,000`
可以用 : 可选参数，见下：

```
scheme@(guile-user)> (format #f "~:d" 8123456789)         ;默认每三位插入一个逗号
"8,123,456,789"
scheme@(guile-user)> (format #f "~,,'*:d" 8123456789)     ;也可以指定插入字符，这里为 *，若未指定，默认为逗号
"8*123*456*789"
scheme@(guile-user)> (format #f "~,,'*,4:d" 8123456789)   ;也可以指定每隔几位插入指定字符
"81*2345*6789"
```

我们也可以组合运用以上选项：

```
scheme@(guile-user)> (format #f "~15,'*,'/,4:@d" 987654321)
"***+9/8765/4321"
```

* `~r`: 指定进制输出指令,输出数值对应的英文单词或罗马数字

```
scheme@(guile-user)> (format #f "~7r" 16)       ;七进制，可指定进制范围：2～36，0-9 和 a-z
"22"
scheme@(guile-user)> (format #f "~r" 16)        ;数值的英文单词，就像 10 被写为 ten
"sixteen"
scheme@(guile-user)> (format #f "~:r" -16)      ;数值的英文单词，就像 10 被写为 tenth
"minus sixteenth"
scheme@(guile-user)> (format #f "~@r" 89)       ; roman 形式
"LXXXIX"
scheme@(guile-user)> (format #f "~@:r" 89)      ; old roman 形式
"LXXXVIIII"
```

在设定进制时，也可以向上面的 `~b`,`~o`,`~d`,`~x` 等指令那样控制数值输出的最小宽度及多余宽度填充字符或其他格式，我们可以用 `~r` 指令的可选项 param：
其 param 形式为：`radix,winwidth,padchar,commachar,commawidth`

```
scheme@(guile-user)> (format #f "~3,10,'*,'-,2:@r" 100)   ;三进制输出，输出宽度为10，其余空白用*填充
"**+1-02-01"                                              ;当参数为正数时，显示正号，每2位插入一个 -
```

## 浮点数输出指令

* `~f`: fixed-point float   小数格式
* `~e`: exponential float   科学计数法格式
* `~g`: general float       自动选择小数格式或科学计数法格式
* `~$`: Monetary style fixed-point float   货币打印格式

```
scheme@(guile-user)> (define pi 3.1415926)
scheme@(guile-user)> (format #f "~f" pi)
"3.1415926"
scheme@(guile-user)> (format #f "~e" pi)
"3.1415926E+0"
scheme@(guile-user)> (format #f "~g" pi)
"3.1415926    "
scheme@(guile-user)> (format #f "~$" pi)
"3.14"
```

`~f` 指令的 param: `width,decimals,scale,overflowchar,padchar`

```
scheme@(guile-user)> (format #f "~f" 5)                 ;参数为整数
"5.0"
scheme@(guile-user)> (format #f "~f" "123")             ;参数为字符串
"123.0"
scheme@(guile-user)> (format #f "~f" "1e-1")            ;参数为字符串
"0.1"
scheme@(guile-user)> (format #f "~@f" 0)                ;非负数前显示正号
"+0.0"
scheme@(guile-user)> (format #f "~@f" -1)
"-1.0"
scheme@(guile-user)> (format #f "~5f" 5)                ;指定宽度
"  5.0"
scheme@(guile-user)> (format #f "~6f" -1.5)     
"  -1.5"
scheme@(guile-user)> (format #f "~6,,,,'*f" 23)         ;指定填充字符 *
"**23.0"
scheme@(guile-user)> (format #f "~6f" 1234567.0)
"1234567.0"
scheme@(guile-user)> (format #f "~1,2f" pi)             ;指定小数点后位数
"3.14"
scheme@(guile-user)> (format #f "~1,2f" 1.5)
"1.50"
scheme@(guile-user)> (format #f "~,,2f" pi)             ;指定参数的放大倍数，10的n次方
"314.15926"
scheme@(guile-user)> (format #f "~,,-2f" pi)            ;指数 n 可以为复数
"0.031415926"
scheme@(guile-user)> (format #f "~4,,,'xf" 12345)       ;应当输出字符串长度大于给定宽度4，用4个 x 代替输出 
"xxxx"
scheme@(guile-user)> (format #f "~6,,,'xf" 12345)
"12345."
scheme@(guile-user)> (format #f "~7,,,'xf" 12345)
"12345.0"
```

`~e` 指令的 params 为： `width,mantdigits,expdigits,intdigits,overflowchar,padchar,expchar`

```
scheme@(guile-user)> (format #f "~e" 5000.25)                   ; 参数为数值
"5.00025E+3"
scheme@(guile-user)> (format #f "~e" "123.4")                   ; 参数为字符串
"1.234E+2"
scheme@(guile-user)> (format #f "~e" "1e4")                     ; 参数为字符串
"1.0E+4"
scheme@(guile-user)> (format #f "~@e" 5000.0)                   ; 正号
"+5.0E+3"
scheme@(guile-user)> (format #f "~10e" 1234.0)                  ; 宽度
"  1.234E+3"
scheme@(guile-user)> (format #f "~10,,,,,'*e" 1234.0)           ; 填充字符
"**1.234E+3"
scheme@(guile-user)> (format #f "~,3e" 11111.0)                 ; 底数小数点后位数
"1.111E+4"
scheme@(guile-user)> (format #f "~,8e" 123.0)                   ; 同上
"1.23000000E+2"
scheme@(guile-user)> (format #f "~,,1e" 1.0e99)                 ; 指数位数
"1.0E+99"
scheme@(guile-user)> (format #f "~,,6e" 1.0e99)                 ; 同上
"1.0E+000099"
scheme@(guile-user)> (format #f "~,,,3e" 12345.0)               ; 底数小数点前位数(正数)
"123.45E+2"
scheme@(guile-user)> (format #f "~,,,0e" 12345.0)               ; 底数小数点前为 0
"0.12345E+5"
scheme@(guile-user)> (format #f "~,,,-3e" 12345.0)              ; 底数紧接小数点后 0 的个数(负数)
"0.00012345E+8"
scheme@(guile-user)> (format #f "~6,,,,'xe" 100.0)              ; 当格式化输出字符串宽度小于指定宽度时，正常输出
"1.0E+2"
scheme@(guile-user)> (format #f "~3,,,,'xe" 100.0)              ; 当格式化输出字符串宽度大于于指定宽度时，同指定替代字符替换格式化字符串
"xxx"
scheme@(guile-user)> (format #f "~,,,,,,'ee" 100.0)             ; 在格式化字符串中，用 e 来代替 E (默认为E)
"1.0e+2"
```

`~g` 指令的 params: `width,mantdigits,expdigits,intdigits,overflowchar,padchar,expchar`。

当数值的绝对值大于 `0.1`，并且其小数格式所占空间比科学技术法格式的尾数所占空间小，`~g` 将会采用 `~f` 格式化输出数值，否则采用 `~e` 格式输出数值。

```
scheme@(guile-user)> (format #f "~12,4,2g" 999.0)
"   999.0    "
scheme@(guile-user)> (format #f "~12,4,2g" "100000")
"  1.0000E+05"
```

`~$` 指令的 params: `decimals,intdigits,width,padchar`。

```
scheme@(guile-user)> (format #f "~$" 5)         ; 用小数格式格式化输出数值，默认 2 位小数
"5.00"
scheme@(guile-user)> (format #f "~4$" "2.25")   ; 指定 4 为小数
"2.2500"
scheme@(guile-user)> (format #f "~4$" "1e-2")   ; 参数为字符串
"0.0100"
scheme@(guile-user)> (format #f "~@$" 0)        ; 正号
"+0.00"
scheme@(guile-user)> (format #f "~,3$" 9.5)     ; 指定整数部分的位数
"009.50"
scheme@(guile-user)> (format #f "~,0$" 0.125)   ; 整数部分位数为 0
".13"
scheme@(guile-user)> (format #f "~,,8$" -1.5)   ; 指定格式化输出字符的宽度
"   -1.50"
scheme@(guile-user)> (format #f "~,,8:$" -1.5)  ; ~:$ 把 padchar 插入在符号之后，padchar 默认为空格
"-   1.50"
scheme@(guile-user)> (format #f "~,,8,'.:@$" 3) ; 指定 padchar 为 .
"+...3.00"
```

## 复数输出指令

`~i` 指令的 params: `width,decimals,scale,overflowchar,padchar`。

```
scheme@(guile-user)> (format #f "~i" 1)
"1.0+0.0i"
```
