created: 2014-04-28T20:17:12+08:00
tags: [linux, CLI]

## Example

格式转换

```
ffmpeg -i input.flv output.mp4
ffmpeg -i my_audio.wav  my_audio.mp3
```


录制屏幕

```
ffmpeg  -f x11grab -s wxga -i :0.0 -crf 0  test.mp4
```


剪切视频

```
ffmpeg -ss 00:00:00 -i input.mp4 -vcodec copy -acodec copy -t 00:01:00 output.mp4
ffmpeg -i input.mp4 -ss 5 -t 10 output.mp4
ffmpeg -ss 5 -i input.mp4 -t 10 -c:v copy -c:a copy output.mp4
```


尺寸变换

```
ffmpeg -i input.mp4 -s 640x360 output.mp4
```


图片视频转换／可以添加声音

```
ffmpeg -i %04d.jpg output.mp4
ffmpeg -i input.mp4 %04d.jpg
ffmpeg -i input.mp3 -i %04d.jpg output.mp4
```

改变fps

```
ffmpeg -i input.mp4 -r 30 output.mp4
ffmpeg -r 30 -i input.mp4 output.mp4
```


提取音乐封面图片

```
ffmpeg -i input.mp3 cover.jpg
```


录制摄像头/可以录制声音

```
ffmpeg -f video4linux2 -s 320x240 -i /dev/video0 out.mpg
ffmpeg -f oss -i /dev/dsp -f video4linux2 -s 320x240 -i /dev/video0 out.mpg
```
