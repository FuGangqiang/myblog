date: 2014-11-06 23:15:00
tags: javascript


`this` 在 javascript 的面向对象编程模式中非常重要，
但当用不同的调用形式来函数时，它有不同的意义，这些不同的调用形式包括：

* method invocation
* function invocation
* constructor invocation
* applay invocation


## method invocation

当函数被存为一个对象的属性时，它就是 method，但 method 被调用时，this 就被绑定到此对象。

```javascript
var myCounter = {
    value: 0,
    increment: function(inc) {
        this.value += typeof inc === 'number' ? inc : 1;
    }
};

myCounter.increment();   // myCounter === 1
myCounter.increment(2);  // myCounter === 3
```

## function invocation

但函数不是一个对象的属性时，this 就被绑定到 global object。

```javascript
a = 0;
f = function () {
    this.a += 1;
}

f();   // a === 1
a = 4;
f();   // a === 5
```

当函数被定义到 method 中时，this 仍然被绑定到 global object，
而不是外层 method 中 this 的含义（外层 method 中 this 被绑定到 method 的 object）。
这个地方非常容易出错。


## Constructor invocation

但函数使用 new 前缀被调用时，它就是一个 constructor，调用后会返回一个对象以此函数的 prototype 作为它的 prototype。
在此种函数中的 this 就被绑定为调用后返回的对象（此对象在函数调用时生成，并用此函数通过 this 初始化）。

```javascript
 Counter = function () {
     this.value = 2;
 }

 myCounter = new Counter();  // myCounter.value === 2
```

## applay invocation

applay 方法可以使一个数组中元素作为参数来调用函数，apply 有两个参数，第一个参数会被绑定到函数中的 this，第二个参数就是一个包含所有函数参数的数组。

```javascript
var myCounter = {
    value: 2
}

inc = function () {
    this.value += 1;
}

inc.apply(myCounter)   // myCounter.value === 3
```
