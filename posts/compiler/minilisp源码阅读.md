created: 2022-05-29T20:53:00+08:00
tags: [lisp, 源码, 编译原理]


## minilisp 简介

minilisp 是一个简单的 lisp 解释器，由 [Rui Ueyama](https://github.com/rui314) 实现，
大约 1000 行 C 语言代码，注释很详细，再抛去空行，仅仅 700 多行, 
代码极为精简，REPL(Read-Eval-Print-Loop) 的执行过程也极为清晰简单，
阅读 minilisp 源码，再次惊叹于 lisp 的`简单`哲学。

minilisp 源码地址：[https://github.com/rui314/minilisp](https://github.com/rui314/minilisp)

minilisp 实现的功能有：

- 支持基本类型有空值、整数(integer)、符号(symbol)、序对(cons cells)
- 词法(lexical)作用域
- 闭包(closure)
- if 条件表达式
- while 循环表达式
- 基本函数有：+, -, <, =, eq, println, gensym, macroexpand
- 函数
- 宏
- 复制(copying)算法垃圾收集系统


## 垃圾收集系统

minilisp 垃圾收集系统是复制算法类型，其算法由 cheney 1970 年研究提出，
不同于深度优先遍历复制算法，其扫描复制时采用广度优先遍历。

垃圾收集的 root 也极其巧妙，除了全局变量 `Symbols` 这个符号列表外，
其他的 root 变量全部来自于运行时的函数栈中的临时变量，
更巧妙的是，这些变量可以通过一个单向链表在函数调用栈间进行索引遍历，
这个单向链表的构造是通过下面的 `DEFINEx` 宏进行生成的：

```
#define ROOT_END ((void *)-1)

#define ADD_ROOT(size)                          \
    void *root_ADD_ROOT_[size + 2];             \
    root_ADD_ROOT_[0] = root;                   \
    for (int i = 1; i <= size; i++)             \
        root_ADD_ROOT_[i] = NULL;               \
    root_ADD_ROOT_[size + 1] = ROOT_END;        \
    root = root_ADD_ROOT_

#define DEFINE1(var1)                           \
    ADD_ROOT(1);                                \
    Obj **var1 = (Obj **)(root_ADD_ROOT_ + 1)

#define DEFINE2(var1, var2)                     \
    ADD_ROOT(2);                                \
    Obj **var1 = (Obj **)(root_ADD_ROOT_ + 1);  \
    Obj **var2 = (Obj **)(root_ADD_ROOT_ + 2)
```


## 宏和函数

minilisp 实现代码中最精彩的地方应该是宏和函数的实现竟然是统一的，
这个应该在其他的 lisp 实现中也类似吧。

宏对象、函数对象表示完全一样，唯一不同的是类型标签(type)：

```
static Obj *make_function(void *root, Obj **env, int type, Obj **params, Obj **body) {
    assert(type == TFUNCTION || type == TMACRO);
    Obj *r = alloc(root, type, sizeof(Obj *) * 3);
    r->params = *params;
    r->body = *body;
    r->env = *env;
    return r;
}
```

宏与函数的调用共用了 `apply_func` 实现，不同的是，宏的参数不会提前被计算，宏调用返回的列表会再次被 eval 计算，

```
static Obj *macroexpand(void *root, Obj **env, Obj **obj) {
    if ((*obj)->type != TCELL || (*obj)->car->type != TSYMBOL)
        return *obj;
    DEFINE3(bind, macro, args);
    *bind = find(env, (*obj)->car);
    if (!*bind || (*bind)->cdr->type != TMACRO)
        return *obj;
    *macro = (*bind)->cdr;
    *args = (*obj)->cdr;
    return apply_func(root, env, macro, args);
}

static Obj *apply(void *root, Obj **env, Obj **fn, Obj **args) {
    if (!is_list(*args))
        error("argument must be a list");
    if ((*fn)->type == TPRIMITIVE)
        return (*fn)->fn(root, env, args);
    if ((*fn)->type == TFUNCTION) {
        DEFINE1(eargs);
        *eargs = eval_list(root, env, args);
        return apply_func(root, env, fn, eargs);
    }
    error("not supported");
}
```

更让人意向不到的是，宏和函数调用都是通过 `env` 实现了词法作用域。
