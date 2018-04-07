created: 2012-06-17T16:35:00+08:00
tags: [metapost]

MetaPost 是一门关于绘制图形的编程语言，本文与其他编程语言手册一样，以经典的 "hello world" 开篇，来对 MetaPost 语言进行大致的了解。
以下为 MetaPost 的一个程序文件：hello.mp

```
% hello.mp
% 这里是注释
 
beginfig(1);
  draw (0cm,0cm)--(1cm,0cm)--(1cm,1cm)--(0cm,1cm)--cycle;
endfig;
 
end;
```

## MetaPost 输出文件

在我们的编辑器里，敲入以上文件代码，保存为 hello.mp，然后运行命令：

```
mpost hello
```

mpost 是 MetaPost 的编译器，它会自动识别文件后缀名 "mp",所以我们编译此文件时可以省略文件后缀名，mpost 会把它自动补上。
运行以上命令后，在同一文件夹下会产生 hello.1 和 hello.log 这两个文件，否则就会在终端显示错误提示信息，如果这样，建议你再检查一下你的 hello.mp 文件是否正确输入。
hello.1 就是我们所想得到的图形文件，它是 eps 格式的,这个文件名是 mpost 的自动设置的，我们可以通过 MetaPost 中的 filenametemplate 命令来改变输出文件名的格式，其格式参数类似于 C 语言中的 printf 函数，如下：

```
% hello.mp
% 这里是注释
 
filenametemplate "%j-%c.eps";
 
beginfig(1);
  draw (0cm,0cm)--(1cm,0cm)--(1cm,1cm)--(0cm,1cm)--cycle;
endfig;
 
end;
```

运行 `mpost hello` 后，就得到了一个名为 hello-1.eps 的图形文件，而不是 hello.1，我们可以用 ghostscript 把它打开，它是一个用黑色线条绘制的边长为 1cm 的正方形。
 
## MetaPost 语句及注释

MetaPost 文件里，所有语句均已分号`;`结束，
`%` 代表注释的开始，直至行尾。
在 hello.mp 文件的头两行就是注释内容。
 
## MetaPost 文件结构

一个 MetaPost 程序文件可以输出数个图形文件，每一个图形文件都是在 beginfig 和其相对应的 endfig 之间的命令来生成的，其大致结构如下：

```
<seq>
 
beginfig(1);
    <seq>
endfig;
 
beginfig(2);
    <seq>
endfig;
 
...
 
 
beginfig(n);
    <seq>
endfig;
 
end;
```

在 hello.mp 文件中只有一对 beginfig--endfig，所以我们只得到一个图形文件 hello.1，如果像上面一样，我们就会得到 hello.1，hello.2，...，hello.n 等 n 个图形文件。
 
## MetaPost 中绘制命令 draw 和路径<path>

hello.mp 中的 draw 是 MetaPost 中最常用的绘图命令，其后跟了一个路径<path>，在这里是 `(0cm,0cm)--(1cm,0cm)--(1cm,1cm)--(0cm,1cm)--cycle`，
其中二元运算符 `--` 生成一个以其两参数为端点的线段，比如 `(0cm,0cm)--(1cm,0cm)` 生成一个路径<path>，这个路径就是以点`(0cm,0cm)`和点`(1cm,0cm)`为端点的线段，
`--` 也可以将一些点顺序相连生成连续线段，比如 `(0cm,0cm)--(1cm,0cm)--(1cm,1cm)--(0cm,1cm)` 就生成三条线段，它们首尾顺序相连，
如果想产生一个闭合路径<path>，将路径<path>首尾两点相连，就要用到 `cycle` 命令，
hello.mp 中，`(0cm,0cm)--(1cm,0cm)--(1cm,1cm)--(0cm,1cm)--cycle` 中的 cycle 就表示该路径起点`(0cm,0cm)`和终点`(0cm,1cm)`相连。
 
## MetaPost 点<point>

MetaPost 中的点<point>是由一个二元组<pair>来表示的，在 hello.mp 中，`(0cm, 1cm)` 就代表 `x` 坐标为 0cm 而 `y` 坐标为 1cm 的点。
 
## MetaPost 单位

MetaPost 的默认单位为 Postscript points，因为 Postscript points 比打印机的 points 略大些，也被称为 "big points"，
Postscript points 和 打印机的 points 的简写形式分别为 bp 和 pt,在 TeX 中其关系如下：

```
1 bp = 1/72 inches
1 pt = 1/72.27 inches
```

当然，MetaPost 也理解其他单位，cm(厘米)、mm(毫米)、in(英寸)和 pc(picas:1pc = 1/6in) 等，
这里给出的单位其实都是 MetaPost 内部定义的数值常量，比如 cm 常量就是数值 28.34645， 1cm 其实是 1*cm 的简写形式，也就是数值 28.34645，
注意，这里的 1cm 其实是一个数值，数值的单位就是 MetaPost 的默认单位 bp。
我们也可以任意自定义一个“单位”，比如 u:=2cm，以后我们就可以用这个单位来表示一个点，

```
(1u, 2u) = (1*2cm, 2*2cm) = (2cm, 4cm) = (56.6929, 113.3858) = (56.6929bp, 113.3858bp)
```
 
## 退出 MetaPost

在 MetaPost 程序文件中，如果最后没有 end 命令，当运行 `mpost <filename>` 命令后，MetaPost 运行 <filename> 程序文件后，不会自动退出，而是进入一个命令行解释器中，在其中我们可以运行 MetaPost 所允许的命令语句,在其中键入 end; 语句后退出解释器。
