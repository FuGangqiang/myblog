created: 2015-07-12T17:31:00+08:00
tags: [python]


## 什么是描述符(descriptor)？

只要定义了名为 `__get__`、`__set__`、`__delete__` 中任意一个方法的对象都叫描述符，
也可以说它实现了描述符协议。

描述符协议方法如下：

```
descr.__get__(self, obj, type=None) --> value
descr.__set__(self, obj, value) --> None
descr.__delete__(self, obj) --> None
```


## 描述符的类型

* 数据性描述符：有 `__get__` 和 `__set__` 方法
* 非数据性描述符：有 `__get__` 方法，没有 `__set__` 方法

描述符的这两种类型和对象字典具有不同的访问顺序，其优先级为：数据性描述符 > 对象字典 > 非数据性描述符。


## 描述符方法的调用

描述符自身就可以调用其描述符方法：`d.__get__(obj)`。

但是对象属性访问通常会自动调用描述符方法。
比如：`obj.d`，用来获取 `obj` 对象中 `d` 属性，
它的访问顺序是：

* `obj.__dict__['d']`
* `type(obj).__dict__['d']`
* `type(obj).__bases__` 基类中访问 `d`

如果访问结果不是描述符，则直接将其返回。

如果访问结果是一个描述符，就会根据描述符类型确定访问优先级，进而决定是否调用描述符方法。
例如数据性描述符优先级高于对象字典的访问，所以会优先调用描述符方法：

* `obj.d` 调用 `type(obj).__dict__['d'].__get__(obj, type(obj))` 方法
* `obj.d = val` 调用 `type(obj).__dict__['d'].__set__(obj, val)` 方法
* `del obj.d` 调用 `type(obj).__dict__['d'].__del__(obj)` 方法


如果是是类属性访问：

* `C.d` 调用 `C.__dict__['d'].__get__(None, C)` 方法


python中的 `property`、`static method`、`class method`、`super()` 都是用描述符来改变属性访问来实现的。


## cached_property 实现

在许多web框架中都会用到`cached_property`装饰器，其实它就是一个描述符类：

```python
class cached_property(object):
    def __init__(self, func):
        self.__doc__ = getattr(func, '__doc__')
		self.__name__ = getattr(func, '__name__')
		self.__module__ = getattr(func, '__module__')
        self.func = func

    def __get__(self, obj, cls):
        if obj is None:
            return self
        value = obj.__dict__[self.func.__name__] = self.func(obj)
        return value
```

`cached_property`用法如下：

```python
class C(object):
    @cached_property
    def x(self):
        return 1
```

这样，我们就可以用`c = C()`定义一个对象，
通过`c.x`来调用描述符，进而使`c.__dict__['x']=1`，
如果我们以后再次访问`c.x`时，我们就可以直接返回`c.__dict__['x']`了。

从上面可以看出：
只有首次访问`c.x`时才调用描述符，
进而生成`c.__dict__['x']`，
往后再次访问就直接返回`c.__dict__['x']`了。
只要我们没有访问`c.x`，在`c.__dict__`中就不会存在`x`键值，进而达到节省内存的作用。


## property 实现

```python
class Property(object):
    def __init__(self, fget=None, fset=None, fdel=None, doc=None):
        self.fget = fget
        self.fset = fset
        self.fdel = fdel
        if doc is None and fget is not None:
            doc = fget.__doc__
        self.__doc__ = doc

    def __get__(self, obj, objtype=None):
        if obj is None:
            return self
        if self.fget is None:
            raise AttributeError("unreadable attribute")
        return self.fget(obj)

    def __set__(self, obj, value):
        if self.fset is None:
            raise AttributeError("can't set attribute")
        self.fset(obj, value)

    def __delete__(self, obj):
        if self.fdel is None:
            raise AttributeError("can't delete attribute")
        self.fdel(obj)

    def getter(self, fget):
        return type(self)(fget, self.fset, self.fdel, self.__doc__)

    def setter(self, fset):
        return type(self)(self.fget, fset, self.fdel, self.__doc__)

    def deleter(self, fdel):
        return type(self)(self.fget, self.fset, fdel, self.__doc__)
```


## static method 实现

```python
class StaticMethod(object):
 def __init__(self, f):
      self.f = f

 def __get__(self, obj, objtype=None):
      return self.f
```


## class method 实现

```
class ClassMethod(object):
     def __init__(self, f):
          self.f = f

     def __get__(self, obj, klass=None):
          if klass is None:
               klass = type(obj)
          def newfunc(*args):
               return self.f(klass, *args)
          return newfunc
```
