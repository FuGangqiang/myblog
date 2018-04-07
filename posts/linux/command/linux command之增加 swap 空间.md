created: 2015-05-20T17:22:14+08:00
tags: [linux, CLI]


```
dd if=/dev/zero of=/path/to/swapfile bs=1M count=32  # 32M
sync
mkswap /path/to/swapfile
sync
chmod 0600 /swap
swapon /path/to/swapfile
```
