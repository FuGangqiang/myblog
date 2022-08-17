created: 2022-08-17T11:29:00+08:00
tags: [forth, 源码, 编译原理]


最近涉猎了一下 forth 编程语言，为了理解这个编程语言，
阅读了一个简单的 forth 解释器源码实现，
代码精简到极致，这也许就是 forth 语言的魅力，
地址为 [https://github.com/GithubPrankster/forward](https://github.com/GithubPrankster/forward)。


## forward 简介

forward 是一个简单的 forth 语言解释器，大约 160 行 C 语言代码，代码清晰简单，
过去一直认为最简单的编程语言是 lisp，了解 forth 语言后，忽然发现，forth 语言比 lisp 还简单，
山外有山啊！

forward 实现功能有：

- 基本类型只有整数
- 所有内置操作有`+`, `-`, `*`, `/`, `.`, `:`, `DUP`, `POP`, `SPACES`, `CR`, `EMIT`, `SWAP`
- 字典和堆栈都是用数组来实现

```
typedef struct{
    char name[64];
    char buf[128];
}expr;

expr funcs[16];
int funcnt = 0;

int stack[16];
int stkcnt = 0;
```

- main 函数就是一个简单的 REPL(Read-Eval-Print-Loop)

```
int main(void)
{
    printf("FORWARD ver. 0.4, by Uneven Prankster @ 2020\n");
    char buf[256];
    while(fgets(buf, sizeof buf, stdin)){
        eval(buf);
        printf(" cool.\n");
    }
    return 0;
}
```

遗憾的是，这个解释器实现没有实现 if 分支，和 loop 循环。
