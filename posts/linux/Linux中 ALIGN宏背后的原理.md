created: 2011-06-13T17:46:00+08:00
tags: [linux]

## 内核宏

```
#define ALIGN(x,a)      __ALIGN_MASK(x,(typeof(x))(a)-1)  
#define __ALIGN_MASK(x,mask)    (((x)+(mask))&~(mask))  
```

不考虑类型，上述代码可以简化为如下：

```
#define ALIGN(x,a)    (((x)+(a)-1)&~(a-1)) 
```

## 原理

```
int a;
int size = 8;
```

如果让`a`为(size=8)的整数倍表示成二进制应是什么样子呢？
那就是让这个数表示成二进制时的最后三位为0.
而要达到这一目标，只要下面这个数与它进行与运算就可以了:

11111111 11111111 11111111 11111000

而上面这个数实际下就是 `~ (size - 1)`，可以将该数称为size的对齐掩码size_mask.   
    
可这样做求出的是比`a`小的那个最大的8的倍数. 如果要求出比`a`大的是不是需要加上8就可以了? 
可是如果a本身就是8的倍数, 这样加8不就错了吗, 所以在`a`基础上加上 `size - 1`, 然后与`size`的对齐掩码进行与运算.
这样, 我们可以定义下面的宏, 用于计算一个数`a`以`size`为倍数的前后两个值：

```
#define alignment_down(a, size) (a & (~(size-1)) )
#define alignment_up(a, size) ((a+size-1) & (~ (size-1)))
```

例如: 

```
a=0, size=8,  则alignment_down(a,size)=0, alignment_up(a,size)=0.
a=6, size=8,  则alignment_down(a,size)=0, alignment_up(a,size)=8.
a=8, size=8,  则alignment_down(a,size)=8, alignment_up(a,size)=8.
a=14, size=8, 则alignment_down(a,size)=8, alignment_up(a,size)=16.
```

注意：size应当为2的n次方, 即2, 4, 8, 16, 32, 64, 128, 256, 1024, 2048, 4096.....


## 在linux中的应用

上面的计算方法在linux等代码中也常常可以看到,下面给出几个例子:

* 当分配地址addr时, 要将该地址以size为倍数对齐, 而且要得到是比addr大的值, 则使用_ALIGN宏：
    ```
    #define _ALIGN(addr,size) (((addr)+(size)-1)&(~((size)-1)))
    ```
* 与页面对齐相关的宏
    ```
    #define PAGE_SIZE         4096
    #define PAGE_MASK         (~(PAGE_SIZE-1))
    #define PAGE_ALIGN(addr) -(((addr)+PAGE_SIZE-1) & PAGE_MASK)
    ```
* 与skb分配时对齐相关的宏
    ```
    #define SKB_DATA_ALIGN(X) (((X) + (SMP_CACHE_BYTES - 1)) & ~(SMP_CACHE_BYTES - 1))
    ```
