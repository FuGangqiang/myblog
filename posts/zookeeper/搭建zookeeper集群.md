created: 2018-04-16T11:06:18+08:00
tags: [zookeeper, 分布式]

[zookeeper][] 是一个为分布式应用提供高效、高可用的分布式协调服务，
提供了诸如数据发布/订阅、负载均衡、命名服务、分布式协调/通知和分布式锁等分布式基础服务。

[zookeeper]: https://zookeeper.apache.org/

本博客讲解 debian9.4 系统下 zookeeper 集群的搭建，集群机器 ip 如下：

- `10.0.0.2`
- `10.0.0.3`
- `10.0.0.4`

## 安装包

分别在集群机器上安装 java 环境包和 zookeeperd 包：

```
apt install openjdk-8-jre-headless
apt install zookeeperd
```

## 创建工作目录

```
mkdir -p /var/lib/zookeeper
mkdir -p /var/log/zookeeper
```

## 配置 zookeeper myid

zookeeper 利用 `myid` 来唯一标识 zookeeper 节点，从而利用它来辨别集群中的每一个节点，
因此集群中的每一个 zookeeper 节点的 `myid` 不能重复出现，
我们将 `myid` 写在 `/var/lib/zookeeper/myid` 文件中：

在此将三台机器的 ip 尾数作为 zookeeper 节点的 `myid`，比如 `10.0.0.3` 节点的 `myid` 为 `3`，
因此在 `10.0.0.3` 机器上的 `/var/lib/zookeeper/myid` 文件内容为：

```
3
```

## 配置 zookeeper zoo.cfg

每一个节点机器上都需要配置 `/etc/zookeeper/conf/zoo.cfg` 文件，配置如下：

```
dataDir=/var/lib/zookeeper
dataLogDir=/var/log/zookeeper
tickTime=2000
clientPort=2181
initLimit=5
syncLimit=2
autopurge.purgeInterval=48
autopurge.snapRetainCount=10

server.2=10.0.0.2:2888:3888
server.3=10.0.0.3:2888:3888
server.4=10.0.0.4:2888:3888
```

## 配置 systemd service

zookeeper 的 service 已经被安装在 `/lib/systemd/system/zookeeper.service` 中，
内容如下：

```
[Unit]
Description=Coordination service for distributed applications
After=network.target
ConditionPathExists=/var/lib/zookeeper/myid

[Service]
Type=simple
User=zookeeper
SyslogIdentifier=zookeeper
EnvironmentFile=/etc/zookeeper/conf/environment
EnvironmentFile=-/etc/default/zookeeper
ExecStart=/usr/bin/java -cp ${CLASSPATH} $JAVA_OPTS -Dcom.sun.management.jmxremote -Dcom.sun.management.jmxre
SuccessExitStatus=143
# ZooKeeper is "fail-fast", see https://zookeeper.apache.org/doc/r3.4.8/zookeeperAdmin.html#sc_supervision
Restart=always

[Install]
WantedBy=multi-user.target
```

在每一台机器上运行开启 zookeeper：

```
systemctl start zookeeper
```

可以将 zookeeper 设为开机自启动：

```
systemctl enable zookeeper
```

## 检测 zookeeper

随便 ssh 进入一台机器，运行以下命令查看 zookeeper 集群健康情况：

```
echo mntr | nc 127.0.0.1 2181
```

类似四字命令也有：


- conf
- cons
- crst
- dump
- envi
- ruok
- srst
- srvr
- stat
- wchs
- wchc
- wchp

具体详情请看：[zookeeper Admin 文档][]。

[zookeeper Admin 文档]: https://zookeeper.apache.org/doc/current/zookeeperAdmin.html


## 测试 zookeeper 命令

开启 zookeeper cli 客户端：

```
cd /usr/share/zookeeper
./bin/zkCli.sh -server 127.0.0.1
```

进入 cli 环境后就可以测试各种 zookeeper 命令了：

```
[zk: 127.0.0.1(CONNECTED) 0] help
ZooKeeper -server host:port cmd args
	stat path [watch]
	set path data [version]
	ls path [watch]
	delquota [-n|-b] path
	ls2 path [watch]
	setAcl path acl
	setquota -n|-b val path
	history 
	redo cmdno
	printwatches on|off
	delete path [version]
	sync path
	listquota path
	rmr path
	get path [watch]
	create [-s] [-e] path data acl
	addauth scheme auth
	quit 
	getAcl path
	close 
	connect host:port
```
