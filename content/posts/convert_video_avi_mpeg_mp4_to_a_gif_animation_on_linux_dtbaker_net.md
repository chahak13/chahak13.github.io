+++
title = "Convert video AVI/MPEG/MP4 to a Gif animation on Linux | dtbaker.net"
author = ["Chahak Mehta"]
draft = false
+++

Convert the video file to a series of small images:

```shell
mkdir /tmp/gif/
ffmpeg -i YOURVIDEOFILE.mp4 -r 10 -s 711×400 /tmp/gif/out%04d.gif
```

Combine these images together into a GIF animation:

```shell
gifsicle -–delay=10 -–loop --optimize /tmp/gif/*.gif > animation.gif/
```
