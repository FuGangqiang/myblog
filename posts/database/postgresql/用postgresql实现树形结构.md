created: 2016-09-27T18:15:39+08:00
tags: [postgresql, 数据库]


关系型数据库对树形结构没有一个很好的解决方案，本文针对 postgresql 数据库，列出以下解决方案:

* jsonb 类型
* xml 类型
* 邻接表(Adjacency List)
* 路径枚举(The Path to a Node)
* 先根遍历树(Modified Preorder Tree Traversal)

本文所有代码均以下图为例：

![foods tree](/media/postgresql/foods_tree.gif)


## jsonb

postgresql 自从 9.4 版本加入 jsonb 类型，用其表示树形结构很方便：

```sql
create table foods (
    food jsonb
);

insert into foods values (
    '{
	     "Fruit": {
		     "Red": [
			     {
				     "name": "Cherry"
				 }
			 ],
			 "Yellow": [
			     {
				     "name": "Banana"
				 }
			 ]
		 },
		 "Meat": [
		     {
			     "name": "Beef"
			 },
			 {
			     "name": "Pork"
			 }
		 ]
	 }'
);
```

postgresql 提供了大量的对 jsonb 类型进行查询、修改操作。


## xml

xml 本身就是一个树形结构，postgresql 内部已经实现了 xml 类型，用它来表示树形结构也是可以的：

```sql
create table foods (
    food xml
);

insert into foods values (
    '<Food>
	    <Fruit>
		    <Red>
			    <name>Cherry</name>
			</Red>
			<Yellow>
			    <name>Banana</name>
			</Yellow>
		</Fruit>
		<Meat>
		    <name>Beef</name>
			<name>Pork</name>
		</Meat>
	 </Food>'
);
```

postgresql 也针对 xml 类型提供了许多 xml 相关的操作函数。


## 邻接表

邻接表就是把所有的节点都放在一张表中，用一个属性记录其父节点：

```sql
create table foods (
    id integer,
	name varchar(15),
	parent integer
);

insert into foods(id, name) values (1, 'Food');
insert into foods(id, name, parent) values (2, 'Fruit', 1);
insert into foods(id, name, parent) values (3, 'Meat', 1);
insert into foods(id, name, parent) values (4, 'Red', 2);
insert into foods(id, name, parent) values (5, 'Yellow', 2);
insert into foods(id, name, parent) values (6, 'Cherry', 4);
insert into foods(id, name, parent) values (7, 'Banana', 5);
insert into foods(id, name, parent) values (8, 'Beef', 3);
insert into foods(id, name, parent) values (9, 'Pork', 3);
```

遍历树形结构可以用通用表表达式(CTE, Common Table Expressions)来实现：

```sql
with recursive tree as (
        select id, name
        from foods
        where id = 1
    union all
        select origin.id, tree.name || '>' || origin.name
        from tree join foods origin
             on origin.parent = tree.id
)
select * from tree;
 id |           name
----+--------------------------
  1 | Food
  2 | Food>Fruit
  3 | Food>Meat
  4 | Food>Fruit>Red
  5 | Food>Fruit>Yellow
  8 | Food>Meat>Beef
  9 | Food>Meat>Pork
  6 | Food>Fruit>Red>Cherry
  7 | Food>Fruit>Yellow>Banana
```


## 路径枚举

路径枚举就是在 foods 表中设置一个 path 属性，用来存储从根节点到当前结点的路径，用分隔符隔开，
尽管 path 可以是 text 类型，但是 postgresql contrib 中的 ltree 模块专门针对这种情况定义了 ltree 类型：

```sql
create extension ltree;
create table foods (
	path ltree
);

insert into foods values('Food');
insert into foods values('Food.Fruit');
insert into foods values('Food.Meat');
insert into foods values('Food.Fruit.Red');
insert into foods values('Food.Fruit.Yellow');
insert into foods values('Food.Fruit.Red.Cherry');
insert into foods values('Food.Fruit.Yellow.Banana');
insert into foods values('Food.Meat.Beef');
insert into foods values('Food.Meat.Pork');
```


## 先根遍历树

用两个数字(left 和 right)对每个节点进行编码如下：

```sql
create table foods (
    name text,
	left integer,
	right integer
);

insert into foods values('Food', 1, 18);
insert into foods values('Fruit', 2, 11);
insert into foods values('Meat', 12, 17);
insert into foods values('Red', 3, 6);
insert into foods values('Yellow', 7, 10);
insert into foods values('Cherry', 4, 5);
insert into foods values('Banana', 8, 9);
insert into foods values('Beaf', 13, 14);
insert into foods values('Pork', 15, 16);
```


![foods preorder tree](/media/postgresql/foods_preorder_tree.gif)

规则如下：

* left 的数值小于该节点的所有后代的 left
* right 的数值大于该节点的所有后代 right
