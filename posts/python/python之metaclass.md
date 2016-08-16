date: 2015-08-05 15:08:00
tags: python


## 什么是metaclass

在 python 中，一切皆是对象，包括类。
对象均是由与其对应的类创建的，既然类也是对象，那么类是由与其对应的类创建的，
此处用来创建类的类就被称为元类，也就是本文所要讲的 metaclass。

```python
>>> class Foo(object):
...     pass
...
>>> f = Foo()
>>> type(f)
<class '__main__.Foo'>
>>> type(Foo)
<class 'type'>
```

上面，首先我们定义了一个名为 `Foo` 的类，用其生成了一个对象实例 `f`，
我们可以用 `type(obj)` 来查看某个对象的类，可以看出，`f` 的类是 `Foo`，`Foo` 的类是 `type`，
而 `type` 就是我们所说的 metaclass。

有人也许会问 `type` 的类又是什么呢？

```python
>>> type(type)
<class 'type'>
```

`type` 的类是 `type` 自己，也可以说 `type` 是 python 对象系统的源头，其底层实现请看 python 源码中的 `PyType_Type` 的定义。


## class 的创建

在介绍 metaclass 之前，我们先来看看在 python 中类的创建过程，如下：

```python
class Bar(object):
    def __init__(self, name):
        self.name = name

    def print_name():
        print(self.name)
```

上面示例通过 `class` 语句定义了一个 `Bar` 类，不仅如此，我们还可以用 `type` metaclass 来创建它：

```python
cls_name = 'Foo'
cls_bases = (object,)
cls_body = '''
def __init__(self, name):
    self.name = name

def print_name(self):
    print(self.name)
'''
cls_attrs = {}
exec(cls_body, globals(), cls_attrs)

Foo = type(cls_name, cls_bases, cls_attrs)
```

上面示例展示了用 `type` metaclass 动态生成类的用法。


在用 `class` 语句自定义类时，默认 metaclass 是 `type`，我们也可以指定 metaclass 来创建类。
由于 python3 和 python2 在指定类的 metaclass 语法不兼容，下面分别示例 python2 和 python3 两个版本。

python2 版本：

```python
class Bar(object):
    __metaclass__ = MetaClass

    def __init__(self, name):
        self.name = name

    def print_name():
        print(self.name)
```

python3 版本：

```python
class Bar(object, metaclass=MetaClass):
    def __init__(self, name):
        self.name = name

    def print_name():
        print(self.name)
```

通过上面指定 metaclass 相关属性，python 解释器就会自动用 `MetaClass` 来创建 `Bar` 类了。


## metaclass 自定义

metaclass 的主要目的就是为了当创建类时能够自动地改变类，
我们可以自定义一些 metaclass，进而改变创建类的行为。

自定义 metaclass 一般是通过 `__new__` 和 `__init__` 方法来改变类的创建的：

```python
class Metaclass(type):
    def __new__(cls, name, bases, attrs):
        # some code

    def __init__(self, name, bases, attrs):
        # some code
```

当然，有时是通过 `__call__` 方法来自定义类：

```python
class Metaclass(type):
    def __call__(cls, *args, **kargs):
        # some code
```


### DocMeta

写代码时，我们通常规定：自定义类中所有方法必须有文档。
如果由程序员自己检查，不免会出现遗漏的地方，因此我们自定义一个 `DocMeta` 来自动检测。

如下：

```python
class DocMeta(type):
    def __init__(self, name, bases, attrs):
        for key, value in attrs.items():
            if key.startswith('__'):
                continue
            if not hasattr(value, '__call__'):
                continue
            if not getattr(value, '__doc__'):
                raise TypeError('%s must have a docstring' % key)
        super(DocMeta, self).__init__(self, name, bases, attrs)
```

这样就可以用 `DocMeta` 来检测自定义类中方法是否有文档，如果没有，就会抛出 `TypeError` 异常：

```python
>>> class Car(object, metaclass=DocMeta):
...     def __init__(self, value):
...         self.value = value
...
...     def change(self):
... 	    self.value += 1
...
TypeError: change must have a docstring
```


### SingletonMeta

有时我们会有这样的需求：由同一个类创建的对象均指向同一个对象。
其实我们也可用 mataclass 实现这样的功能：

```python
class SingletonMeta(type):
    instance = None
    def __call__(cls, *args, **kwargs):
        if not cls.instance:
             cls.instance = super(SingletonMeta, cls).__call__(*args, **kwargs)
        return cls.instance

class Singleton(metaclass=SingletonMeta):
    pass

>>> a = Singleton()
>>> b = Singleton()
>>> a is b
True
```

上面代码中，当我们调用 `Singleton()` 来创建对象 `a` 和 `b` 时，解释器就会调用 `SingletonMeta.__call__` 方法来保证创建对象的唯一性。


## metaclass应用

python 界的领袖 Tim Peters 说过：

> 元类就是深度的魔法，99% 的用户应该根本不必为此操心。
> 如果你想搞清楚究竟是否需要用到元类，那么你就不需要它。
> 那些实际用到元类的人都非常清楚地知道他们需要做什么，而且根本不需要解释为什么要用元类。

元类的主要用途是创建 API。一个典型的例子是 Django ORM。它允许你像这样定义：

```python
class Person(models.Model):
    name = models.CharField(max_length=30)
    age = models.IntegerField()
```

但是如果你像这样做的话：

```python
guy = Person(name='bob', age='35')
print(guy.age)
```

这并不会返回一个 IntegerField 对象，而是会返回一个 int，甚至可以直接从数据库中取出数据。
这是有可能的，因为 models.Model 定义了 metaclass，并且使用了一些魔法能够将你刚刚定义的简单的 Person 类转变成对数据库的一个复杂 hook。
Django 框架将这些看起来很复杂的东西通过暴露出一个简单的使用元类的 API 将其化简，通过这个 API 重新创建代码，在背后完成真正的工作。
