created: 2010-12-06T14:36:00+08:00
tags: [scheme]

Guile-dbi(data base interface) 为 guile 提供了一种简单、通用和易于使用的各种数据库接口, 例如, Postgres, MySQL 或者是 SQLite.

guile-dbi 被分为两部分: DBI(database independent) 和 DBD(database dependent) 插件, DBI 为 guile提供操作接口, 而 DBD 插件为 scheme 连接特定的 SQL server. 现在, 只有 Postgres, MySQL 和 SQLite3 的 DBD 后端.

## 历史

2004 年, Maurizio Boriani 写道:

> 我在寻找一个为 guile scheme 提供通用的 database 库, 虽然找到了一些, 但是他们并不是真正的 'dynamic', 只是几个不同的后端简单的编译而成, 而我寻找的是像 perl, php, tcl等语言的 dbi 系统, 其可以在运行时连接 database driver, 最后没有寻找到, 因此我就写了一个.

2008 年, Linas Vepstas 写道:

> 我在寻找一个为 guile scheme 提供通用的 database 库, 并且这是我找到的唯一具有文档说明, 能够保证工作的一个. 经过一些补丁后, 满足了我的需求, 现在我就在我的 OpenCog NLP 子系统工程里使用它.

## 获取 Guile DBI

你可以从 [https://github.com/eestrada/guile-dbi][] 网页获得 Guile dbi 最新版本.

## 使用手册

scheme 接口非常简单, 只有 5 个函数: dbi-open, dbi-close, dbi-query, dbi-get_status 和 dbi-get_row.

只要有相应的 dbd backend, guile DBI 可以支持任何数据库, 目前只有 MySQL, Postgres 和 SQLite 后端.

下面是一个 SQLite3 的使用实例:

```
(use-modules (dbi dbi))
 
;; Log into the database.
(define db-obj (dbi-open "sqlite3" "my-example-db"))
 
;; Create a table.
(dbi-query db-obj "create table hellotable(id int, name varchar(15))")
 
;; Look at the return status of the last SQL command
(display db-obj) (newline)
 
;; Populate the table with values.
(dbi-query db-obj "insert into hellotable ('id', 'name') values('33', 'ola')")
(dbi-query db-obj "insert into hellotable ('id', 'name') values('34', 'dzien dobre')")
(dbi-query db-obj "insert into hellotable ('id', 'name') values('44', 'annyong haseyo')")
(display db-obj) (newline)
 
;; Display each of the rows of the table, in turn.
(dbi-query db-obj "select * from hellotable")
(display db-obj) (newline)
(write (dbi-get_row db-obj)) (newline)
(write (dbi-get_row db-obj)) (newline)
(write (dbi-get_row db-obj)) (newline)
(write (dbi-get_row db-obj)) (newline)
 
;; Close the database.
(dbi-close db-obj)
(display db-obj)(newline)
```

下面是使用 MySQL 数据库的一个实例. 这个实例假定 MySQL 服务已经运行, 并且一个名为 pippo 的 table 已经被创建, 并被存储了一些数据:

```
#!/usr/bin/guile -e main -s
!#
 
(use-modules (dbi dbi))
  
(define ciccio (dbi-open "mysql" "user:pass:pluto:tcp:localhost:3306"))
(define ret #f)
;; (define ciccio (dbi-open "mysql" "user:pass:pluto:socket:/tmp/mysql.sock"))
 
(define main
  (lambda (args)
    (display "HERE")(newline)
    (display ciccio)(newline)
    (dbi-query ciccio "select * from pippo")
    (display ciccio)(newline)
    (set! ret (dbi-get_row ciccio))
    (let loop ()
         (if (not (equal? ret #f))
             (begin
              (display ret)(newline)
              (set! ret (dbi-get_row ciccio))
              (loop))))
    (display ret) (newline)))
```
