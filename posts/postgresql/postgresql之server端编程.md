created: 2016-09-19T16:32:12+08:00
tags: [postgresql, 数据库]


## 什么是 server 端编程？

postgresql 不仅仅具有数据存储和管理的功能，而且也是一个很强大的编程框架。
在服务端，用户可以自定义函数、数据类型、触发器或重载操作符，
可以交叉调用服务端数种语言的函数或库，处理所有数据类型的数据，即使是非数据相关的任务也是可以的。


## 为什么要用 server 端编程？

一个应用通常有数种语言编写，
有些人将尽可能多的业务逻辑规则放在客户端，比如放在浏览器的 Javascript 代码中，
也有些人将它放在中间层应用服务器来处理业务规则，
那为什么将这些业务逻辑规则放在数据库 server 端呢？

server 端编程有以下几个优点：

1. 减少数据库 server 端 和 client 端的通信开销
1. 性能好，离数据越近，处理数据就会越快
1. 可以隐藏数据库的元信息（表的定义），对应用程序不可见
1. 同一套业务逻辑可被不同应用程序共用，当业务逻辑改变时，只需修改相关 server 端代码就可以了
1. 维护方便，只需写一个 DDL 脚本重新定义那些函数，一旦运行脚本，所有客户端就可见了
1. 安全性，服务端可以用户定义的函数分类，不同用户给它们不同的访问权限，也可以控制每一个函数的数据访问权限

当然使用 server 端编程也有它的劣势：

1. 难于调试
1. 移植性差


## server 端编程语言

postgresql 官方支持的 server 端语言有：

* PL/pgSQL
* PL/TCL
* PL/Perl
* PL/Python

当然还有一些第三方提供的 server 端语言：

* PL/Java
* PL/PHP
* PL/Py
* PL/R
* PL/Ruby
* PL/Scheme
* PL/sh

## PL/pgSQL 语言

PL/pgSQL 相比于其他语言更接近与 SQL 语言，它受 oracle 的 PL/SQL 语言影响很大，
其中 PL 是 Procedural Language 的简写，pgSQL 代表是 postgresql。

使用 PL/pgSQL 有以下优点：

* 易于使用
* postgresql 默认开启
* 对数据密集型的任务进行了充分的优化

一个简单的 PL/pgSQL 定义如下（add 函数）：

```sql
CREATE OR REPLACE FUNCTION add(a INTEGER, b NUMERIC)
RETURNS NUMERIC
AS $$
	SELECT a+b;
$$ LANGUAGE SQL;
```

调用方法如下：

```sql
SELECT add(1,2);
 add
-----
   3
(1 row)

SELECT * FROM add(1,2);
 add
-----
   3
(1 row)
```


#### 块结构

PL/pgSQL 是一个块结构语言，函数定义的所有文本都必须是一个块。
一个块用下面的方法定义：

```sql
[ <<label>> ]
[DECLARE
	declarations]
BEGIN
	statements
END [ label ];
```

* 所有在块中的定义和申明都以分号结束
* 块支持嵌套
* 关键字不区分大小写
* 内层块声明同名变量隐藏外层块声明同名变量，子块可以通过块 lable 前缀访问同名变量

一个完整的 PL/pgSQL 函数定义如下：

```sql
CREATE FUNCTION somefunc() RETURNS integer AS $$
DECLARE
	quantity integer := 30;
BEGIN
	-- Prints 30
	RAISE NOTICE 'Quantity here is %', quantity;
	quantity := 50;

	-- Create a subblock
    DECLARE
    	quantity integer := 80;
    BEGIN
    	-- Prints 80
    	RAISE NOTICE 'Quantity here is %', quantity;
    	-- Prints 50
    	RAISE NOTICE 'Outer quantity here is %', outerblock.quantity;
    END;

    -- Prints 50
	RAISE NOTICE 'Quantity here is %', quantity;
    RETURN quantity;
END;
$$ LANGUAGE plpgsql;
```


#### 控制结构

* return

```sql
RETURN expression;
RETURN NEXT expression;
RETURN QUERY query;
RETURN QUERY EXECUTE command-string [ USING expression [, ... ] ];
```

* conditionals

```sql
IF boolean-expression THEN
    statements
END IF;


IF boolean-expression THEN
    statements
ELSE
    statements
END IF;


IF boolean-expression THEN
    statements
[ ELSIF boolean-expression THEN
    statements
[ ELSIF boolean-expression THEN
    statements
    ...]]
[ ELSE
    statements ]
END IF;


CASE search-expression
    WHEN expression [, expression [ ... ]] THEN
      statements
  [ WHEN expression [, expression [ ... ]] THEN
      statements
    ... ]
  [ ELSE
      statements ]
END CASE;


CASE
    WHEN boolean-expression THEN
      statements
  [ WHEN boolean-expression THEN
      statements
    ... ]
  [ ELSE
      statements ]
END CASE;
```

* loops

```sql
[ <<label>> ]
LOOP
    statements
END LOOP [ label ];


[ <<label>> ]
WHILE boolean-expression LOOP
    statements
END LOOP [ label ];


[ <<label>> ]
FOR name IN [ REVERSE ] expression .. expression [ BY expression ] LOOP
    statements
END LOOP [ label ];


[ <<label>> ]
FOR target IN query LOOP
    statements
END LOOP [ label ];


[ <<label>> ]
FOREACH target [ SLICE number ] IN ARRAY expression LOOP
    statements
END LOOP [ label ];
```


* exception

```sql
[ <<label>> ]
[ DECLARE
    declarations ]
BEGIN
    statements
EXCEPTION
    WHEN condition [ OR condition ... ] THEN
        handler_statements
    [ WHEN condition [ OR condition ... ] THEN
          handler_statements
      ... ]
END;
```


#### 多态

PL/pgSQL 函数的参数和返回值均可以声明为多态，也就是说可以接受任何类型(any*)：

```sql
CREATE OR REPLACE FUNCTION get_array
(IN anyelement, IN anyelement, OUT anyelement, OUT anyarray)
AS $$
	SELECT $1, ARRAY[$1, $2];
$$ LANGUAGE SQL;


SELECT get_array(4,5), get_array('c'::text, 'd'::text);
  get_array  |  get_array
-------------+-------------
 (4,"{4,5}") | (c,"{c,d}")
(1 row)
```


#### 重载

PL/pgSQL 多个函数可以共用一个函数名，但是它们的参数的个数或类型必须不同：

```
CREATE FUNCTION test(int, real) RETURNS ...
CREATE FUNCTION test(smallint, double precision) RETURNS ...
```
