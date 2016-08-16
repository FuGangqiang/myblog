date: 2015-07-19 00:10:00
tags: python


python程序充满了名字(name)，变量名、函数名、类名、模块名等等。
python程序中的名字要么是它的定义，要么是它的引用。

一个名字的作用域(scope)就是它起作用（有意义）的一段程序代码，
在python中，作用域是静态的，即作用域在程序编译时就已经确定，也称为词法作用域。

在python中有四种基本的作用域，`LEGB`，也就是`Local`、`Enclosing`、`Global`、`Builtin`，
除此之外，还有一个特别的作用域，也就是`class body`，其并不会成为内部方法(method)的`Enclosing`作用域，
但是，在其中的任何赋值都会使变量绑定到`class body`。

python3中，`list comprehension`也会产生一个作用域，但python2并不会产生。


## class body

名字的解析顺序，就是我们经常所说的`LEGB`原则，
但是在类中有一点特别之处，也就是前面说的类体(class body)并不会成为其内部方法的`Enclosing`作用域。

看一段代码：

```python
x = 0
class C(object):
    y = x       # 全局变量x
    x = x + 1   # 左边的x是一个class变量，而右边的是引用全局变量x
    z = x       # class变量x

    def method(self):
        print(self.x)    # -> 1
        print(x)         # -> 0, 全局变量x
        print(y)         # -> NameError: global name 'y' is not defined

c = C()
print(c.x, c.y, c.z, x)  # -> (1, 0, 1, 0)
```

在上面`C`的定义中，`y=x`并不会抛出`UnboundLocalError`异常，这也是`class body`有别于`def body`的地方之一。


## list comprehension

`list comprehension`也是python3和python2之间的一个区别，python3中会产生一个作用域，而python2不会。

在python2中会有以下结果：

```
>>> [ i for i in range(5) ]
[0, 1, 2, 3, 4]
>>> i
4
```

而python3中则会抛出`NameError`异常：

```
>>> [ i for i in range(5) ]
[0, 1, 2, 3, 4]
>>> i
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
NameError: name 'i' is not defined
```

