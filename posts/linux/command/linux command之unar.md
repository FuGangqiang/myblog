created: 2019-11-13T15:01:06+08:00
tags: [linux, CLI]


网上下载的一些 zip 文件通常在 linux 下解压会出现文件路径命乱码的问题，需要 `unar` 命令来解压

```
pacman -S unarchiver
unar xxx.zip
```
