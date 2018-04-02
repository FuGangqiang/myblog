created: 2016-03-16T13:27:08+08:00
tags: [redis, 源码]


## 基本数据结构


### 动态字符串(dynamic string)

* sds.h
* sds.c

存储在 heap 上，只是比 c 中的字符串类型多了一个结构头，
在结构头里有 len 和 size 属性，实现了二进制安全。

```c
struct sdshdr {
    unsigned int len;
    unsigned int free;
    char buf[];
};
```


### 双向链表(doubly linked list)

* adlist.h
* adlist.c

与教科书上面讲的几乎没什么差别：

```c
typedef struct listNode {
    struct listNode *prev;
    struct listNode *next;
    void *value;
} listNode;

typedef struct list {
    listNode *head;
    listNode *tail;
    void *(*dup)(void *ptr);
    void (*free)(void *ptr);
    int (*match)(void *ptr, void *key);
    unsigned long len;
} list;
```


### 字典(Hash Table)

* dict.h
* dict.c

利用链地址法来解决字典 hash 碰撞。

redis 中的 dict 的 rehash 是渐进式，
也就是说 rehash 过程不是一下子就完成的，
起初只是将 dict 状态修改为正处于 rehashing 中，
而是在之后的每一次 dict 访问时，都会转移几个 key-value 对，
直到 rehash 完毕，再次修改 dict 状态为已 rehashing 完毕。

```c
typedef struct dictEntry {
    void *key;
    union {
        void *val;
        uint64_t u64;
        int64_t s64;
        double d;
    } v;
    struct dictEntry *next;
} dictEntry;

typedef struct dictht {
    dictEntry **table;
    unsigned long size;
    unsigned long sizemask;
    unsigned long used;
} dictht;

typedef struct dict {
    dictType *type;
    void *privdata;
    dictht ht[2];
    long rehashidx; /* rehashing not in progress if rehashidx == -1 */
    int iterators;
} dict;
```


### 跳跃表

* redis.h
* t_zset.c

跳跃表其实就是有序链表的一个变种，查找、删除和添加元素的速度和平衡二叉树相近，但实现起来更直观一些。

```c
typedef struct zskiplistNode {
    robj *obj;
    double score;
    struct zskiplistNode *backward;
    struct zskiplistLevel {
        struct zskiplistNode *forward;
        unsigned int span;
    } level[];
} zskiplistNode;

typedef struct zskiplist {
    struct zskiplistNode *header, *tail;
    unsigned long length;
    int level;
} zskiplist;
```


### 整数集合

* intset.h
* intset.c

就是用一个数组存放有序整数的集合，根据存放整数的大小来确定集合中各个整数所占用的字节数。

```c
typedef struct intset {
    uint32_t encoding;
    uint32_t length;
    int8_t contents[];
} intset;
```


### 压缩列表

* ziplist.h
* ziplist.c

压缩列表也是一个列表，列表根据各元素的性质来决定使用何种方式、大小来存放元素，
其实就是一段连续内存，其结构如下：

```
<zlbytes><zltail><zllen><entry><entry><zlend>
```

entry 可以存放的类型有：

* 4bit 无符号数
* 1byte 符号整数
* 3byte 有符号整数
* int16_t 整数
* int32_t 整数
* int64_t 整数
* 长度小于等于 63(2^6 - 1) 字节的字符数组
* 长度小于等于 16383(2^14 - 1) 字节的字符数组
* 长度小于等于 4294967295(2^32 - 1) 字节的字符数组

ziplist 中的 entry 的定义如下：

```c
typedef struct zlentry {
    unsigned int prevrawlensize, prevrawlen;
    unsigned int lensize, len;
    unsigned int headersize;
    unsigned char encoding;
    unsigned char *p;
} zlentry;
```


## 基本类型

redis 支持的基本类型及它们在内存中的表现形式：

* string 类型　

    * long
    * 动态字符串

* list 类型

    * 压缩列表
    * 双向类表

* set 类型

    * 整数集合
    * 字典

* zset 类型

    * 跳跃表
    * 双向链表

* hash 类型

    * 压缩列表
    * 字典
