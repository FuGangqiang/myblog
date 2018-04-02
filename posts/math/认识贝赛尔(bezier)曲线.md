created: 2014-02-16T20:51:00+08:00
tags: [asymptote, bezier, 数学]


注：本文摘自网友 F.A.Zhang(cvgmt)的《Notes on asy》

最近在学习 MetaPost 和 Asymptote 绘图，它们画曲线与通常的其他软件不一样，
并非采用折线逼近来生成曲线的办法，而是采用一种称为 Bezier 曲线办法。

理解 MetaPost/Asymptote 这种生成曲线或曲面的方法，
会有助于理解 MetaPost/Asymptote 的与路径 path 相关的一些命令，
也有助于灵活地解决问题。

我们知道如何通过给定两点生成直线段，那如何生成曲线呢?
1959 年，法国人 Paul de Casteljau(与此独立地，1962 年的 Bezier)
发现了现在称为 Bezier 曲线的生成光滑曲线的办法。
我们在此阐述一下其中巧妙的想法。

给出 Z0 和 Z1 两个点，我们只能得到连接它们的直线段，
此时如果给出一个时刻 t，那么我们可以通过这个时刻访问到这条线段上以 t 为比例的点，
即:

<div>
$$
Z=Z_0+t(Z_1-Z_0)
$$
</div>

如图：

![bezier1](/media/asy/bezier1.svg)

产生上图所用 asymptote 代码为：

```
size(３00, 0);
pair Z0 = (0, 0), Z1 = (4, 0);
real t = 1 / 3;
pair Z = interp(Z0, Z1, t);
draw(Z0--Z1);
dot("$Z_{0}$", Z0, S);
dot("$Z_{1}$", Z1, S);
dot("$Z$", Z, N);
dot(Z, red, UnFill);
arrow("$t=\frac{1}{3}$", Z, SE);
```

de Casteljau 天才的想法是添加一个额外的点 C，并且把它称为控制点。
然后我们顺次连接 Z0、C、Z1，只能得到一段折线，
下面我们按照一个比例 t(我们也称为时刻t)，比如 t=1/3，
然后在 Z0 到 C 的 1/3 处取一个点 A，
同样，在 C 到 Z1 的 1/3 处取另外一个点 B，
连接 A 与 B，然后在 A 到 B 的 1/3 处再取点 Z，
那么就称 Z 为对应于这个时刻 1/3 一个点。
我们可以想象，当那些时刻遍历 0 到 1 的所有的时刻，就得到一条轨迹，
通过简单的代数运算，我们可以依次得到

<div>
$$
\begin{align}
A & = (1-t)Z_0+tC \\
B & = (1-t)C+tZ_1 \\
Z & = (1-t)A+tB
\end{align}
$$
</div>

把(1)和(2)代入(3)，我们可以得到 Z 与 t 的关系:

<div>
$$
Z(t) = {(1-t)}^2Z_0 + 2t(1-t)C + t^2Z_1
$$
</div>

这是一条过 Z0 和 Z1 的二次参数曲线，
如下图中的红线：

![bezier2](/media/asy/bezier2.svg)

产生上图所用 asymptote 代码为：

```
import graph;
size(300);
pair Z0 = (0, 0), Z1 = (4, 0), C = (1, 3);
real t = 1 / 3;
pair A = interp(Z0, C, t);
pair B = interp(C, Z1, t);
pair Z = interp(A, B, t);
draw(Z0--C--Z1);
draw(A--B);
pair f(real t) {
    return (1-t)^2 * Z0 + 2t * (1-t) * C + t^2 * Z1;
}
draw(graph(f, 0, 1), red);
dot("$A$", A, W);
dot("$B$", B, NE);
dot("$C$", C, N);
dot("$Z_{0}$", Z0, S+S);
dot("$Z_{1}$", Z1, SE);
dot("$Z$", Z, N);
dot(Z, red, UnFill);
arrow("$t=\frac{1}{3}$", Z, SE);
```

我们上面得到的是 2 次的参数曲线，如果采用两个控制点，那么我们就能得到 3 次的 Bezier 曲线，
其原理是类似的:

<div>
$$
Z = {(1-t)}^3Z_0 + 3t{(1-t)}^2C_0 + 3t^2(1-t)C_1 + t^3Z1
$$
</div>

如图：

![bezier3](/media/asy/bezier3.svg)

产生上图所用 asymptote 代码为：

```
size(300);
pair Z0 = (0, 0), C0 = (1, 1), C1 = (2, 1),  Z1 = (3, 0);
draw(Z0--C0--C1--Z1);
real t = 1 / 3;
pair M0 = interp(Z0, C0, t);
pair M1 = interp(C0, C1, t);
pair M2 = interp(C1, Z1, t);
pair M3 = interp(M0, M1, t);
pair M4 = interp(M1, M2, t);
pair Z = interp(M3, M4, t);
draw(M0--M1--M2);
draw(M3--M4);
draw(Z0..controls C0 and C1..Z1, red);
dot("$Z_{0}$", Z0, SW);
dot("$Z_{1}$", Z1, SE);
dot("$C_{0}$", C0, NW);
dot("$C_{1}$", C1, NE);
dot("$M_{0}$", M0, NW);
dot("$M_{1}$", M1, N);
dot("$M_{2}$", M2, NE);
dot("$M_{3}$", M3, SE);
dot("$M_{4}$", M4, SE);
dot("$Z$", Z, S);
dot(Z, red, UnFill);
arrow("$t=\frac{1}{3}$", Z, SE+E);
```
