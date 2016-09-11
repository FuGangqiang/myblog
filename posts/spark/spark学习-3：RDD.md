date: 2016-09-10 10:36:48
tags: spark


Spark 最主要的抽象就是 RDD(resilient distributed dataset)弹性分布式数据集，
Spark 中所有工作都会被表达为对 RDD 的操作：

* 生成 RDD
* 转换 RDD
* 获取 RDD 结果
* 保存 RDD


## 生成 RDD

#### 加载内置类型

```python
rdd = sc.parallelize(['hello', 'spark'])
```


#### 加载文本文件

```python
rdd = sc.textFile('README.md')
```

#### 加载 json 文本文件

```python
import json

rdd = sc.textFile('filename.json).map(lambda x: json.loads(x))
```

或者

```python
rdd = spark.read.json('filename.json')
```


#### 加载 csv 文本文件

```python
rdd = sc.textFile('filename.csv').map(lambda line: line.split(','))
```

或者

```
rdd = spark.read.csv('filename.csv')
```

#### 加载 SequenceFile

```
rdd = sc.sequenceFile(inFile, 'org.apache.hadoop.io.Text', 'org.apache.hadoop.io.IntWritable')
```


## 转换 RDD

* filter
* map
* flatMap
* distinct
* sample
* union
* intersection
* subtract
* cartesian
* reduceByKey
* foldByKey
* groupByKey
* combineByKey
* mapValues
* flatMapValues
* keys
* values
* sortByKey
* subtractByKey
* join
* rigthOuterJoin
* leftOuterJoin
* cogroup
* partitionBy


## 获取 RDD 结果

* first
* top
* take
* takeOrdered
* takeSample
* collect
* count
* reduce
* fold
* combine
* aggregate
* collectAsMap
* countByValue
* lookup


## 保存 RDD


#### 保存至文本文件

```
rdd.saveAsTextFile(outputFile)
```


#### 保存至 json 文件

```
rdd.map(lambda x: json.dumps(x)).saveAsTextFile(outputFile)
```
