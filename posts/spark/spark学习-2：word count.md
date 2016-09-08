date: 2016-09-08 22:34:20
tags: spark


统计 spark 安装目录里面的 `README.md` 所有单词出现的频率。


## 交互界面

开启交互程序：

```
./bin/pyspark
```

从 `README.md` 文件创造 RDD：

```python
>>> textFile = sc.textFile("README.md")
```

对 `textFile` RDD 进行一系列的转换产生一个新的 RDD：

```python
>>> wordCounts = textFile.flatMap(lambda line: line.split()).map(lambda word: (word, 1)).reduceByKey(lambda a, b: a+b)
```

获取统计结果：

```python
>>> wordCounts.collect()
```


## 应用程序

创建 `word_count.py` 文件如下：

```python
"""Word Count"""
from pyspark import SparkContext

sc = SparkContext("local", "Word Count App")
textFile = sc.textFile("README.md")
wordCounts = textFile.flatMap(lambda line: line.split()).map(lambda word: (word, 1)).reduceByKey(lambda a, b: a+b)
print("README.md word count statistics:", wordCounts.collect())
```

运行命令：

```
bin/spark-submit --master local[4] word_count.py
```
