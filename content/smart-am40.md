在咸鱼上面收了一个 smart am40。打算用它来做 nas 和旁路由。

之前家里有一个斐讯盒子 n1，吃灰吃了比较久。由于投影仪是自带了系统的，做电视盒子不太用得上。当旁路由用过一阵子，主要就是给家里的投影仪和手机用，不过也不算强需求。然后外接我的移动硬盘盒当 nas 用，发现 n1 是 usb2 的接口，传输性能不太行。
所以 nas 移到了我的主力开发机上，我用虚拟机装了个 OMV，然后给虚拟机映射物理硬盘。缺点有两个：一个是我的主机开发机是放在办公桌上的，只有 wifi 连网线不方便，网络的稳定性不够好。另一个缺点是，主力工作机是常压桌面CPU，8c64g，24 小时开机在功耗方面不太环保。所以想换一个其它硬件。

考虑过树莓派，不过树莓派还不如国产派。同价位下，国产派的配置吊打树莓派，可以说同价位至少是领先一代硬件的水平。最新的树莓派只是变得越来越贵，越来越没有性价比，已经没什么玩头了。国产派不如捡垃圾。比如说之前的 n1 的性价比可高了，可以当家庭媒体服务器，可以当游戏机，只是 usb2 接口所以不适合玩 nas。所以我才找寻找替代品。

捡垃圾的几种类型：

- 路由器
- 电视盒子
- 工控机/瘦客户端
- 旧手机/旧电脑或者旧服务器
- 小主机

路由器加一个 usb 口，插上一个硬盘，可不就变成家用 nas 了么。不过现在大多数的路由器都把 usb 口阉割了，故意不让这么玩。其实厂家的出发点主要是路由器的功耗低，usb 供电不稳定影响到产品的使用。对于路由器而言，低功耗和稳定性大于其它, 所以家用路由器 cpu 一般不咋滴，可玩性较低。

电视盒子是非常好玩的一类，基本都是 arm 系列，主要的 cpu 厂商有两家，晶晨和瑞芯微。性能可以去搜一搜电视盒子cpu天梯榜。之前的斐讯n1就是 s905d的 CPU，现在这个 am40 是 rk3399 的 CPU。去小黄鱼搜这些关键字，都可以搜到一些极具性价比的玩具。黑豹x2就是其中一个，不过没有选它，主要因为它是 usb2.0 的，虽然 CPU 比 n1 更好一点，但是没有 usb3 接口是硬伤。要想玩 nas，必须至少两个点要满足：千兆网口和 usb3。

工控机，瘦客户端这类，smart am40 就属于工控机，原本 am40 是用于白板设备里面的。瘦客户端这边可以搜 戴尔wyse 关键字，也能找到一些非常好玩的玩具。

旧手机旧电脑这块就没啥好说的了，收二手的也便宜。家里本身有的话，就是废物再利用。我家里的设备够多了，二手笔记本也有，非要用来搭个家庭服务器也可以，不过我主要想的是把需求精简到很少数的几个设备上面。准确来讲，需求就是两个，一个是家庭 24 小时常开的机器，另一个是我自己的工作主机。

小主机，mini 主机这块，从下限往上限，上限可以完全替代台式机电脑，硬件性能上不封顶。我之前换的[新电脑](new-computer.md)就是小主机，虽然我家的这个块头已经算相对比较大了。nuc 这个关键字，品牌现在国产的都起来了，零刻，铭凡等等等，intel 已经放弃这块市场了。
非常值得一提的是: mac mini。搭配最新的 m4 CPU，现在不到 3000 就可以拿下，真的是非常非常非常吸引人啊，性能强劲而功耗也控制得非常好，吊打 intel。如果我家里一台电脑都没有，所有需求全部整合到一台机器里面，当下让我选我肯定会选 mac mini。
可惜家里已经有 macbook，不差一个 osx 系统；已经有 mini台式机，也不缺硬件性能；还有之前的一个 xps 笔记本，不差旧二手机器，所以 mac mini 真买来也是吃灰。

价位方面，100 以内，100-200 以内的这些玩具，都还是有一点玩头的，超过了就没啥玩头。双11 去捡垃圾可以在 400 块钱搞定 n100 的小主机，平时最低也能大概 500 搞定。这个 smart am40 我可是花了"巨资" 138 才买到的。太贵了不适合垃圾佬的风格。

x86 的常见 CPU，大概就是 j1900 -> j3455 ->  n100，依次性能提升一倍，当然，功耗也是依次提升的。到 n100 的性能基本上可以当桌面系统，做一些轻量的工作了。x86 的 CPU 还有一项主要优势，就是兼容性好，可以 PVE 搞个底坐，上面开各种虚拟机，随机 windows 还是 linux 还是再搞 docker，可玩的花样多。arm 的 CPU 主要优势就是省电，不过兼容性就不如 x86。有些人的说法，nas 玩到尽头都是 x86 的。

对于我自己的需求，可以改用 macbook 当主力开发机了，虽然内存是 16G 的，但基本还算够用。CPU 我实测过完全不输 AMD R7 4750G。所以台式就可以收起来了，让它 24 小时开机不环保，之前都是 ssh 到这个机器上面做开发的，以后直接在 MBP 上面做开发。
新到的 smart am40 最最最重要的点，就是功耗低。不到 6w，24 小时开机也就相当于多放一台路由器的水平，一年到头的电费不超过 30块钱。

--------

## 系统安装

当前(2025-03-15)网上能找到的 am40 的资料要比 n1 要少得多，用相对匮乏来说也不为过。[这里](https://mozi1924.com/2024/06/08/am40_armbian_hassio/)有一篇还算比较有价值。最终它链接到的就是最原始的教程，比如[这里](https://am40.cache.cloudns.org/#smart-technologies-am40)，[这里](https://gitee.com/pylixm/rk3399)，[这里](https://gitee.com/iddddg/rk3399)，[这里](https://gitee.com/xiayang0521/rk3399)，这些都是从同一个地方 fork 出去的，也没看出来 fork 后改了啥，区别是有些网页失效了，自动跳转到其它分支。

最源头的维护应该就是 armbian 和 openwrt 两个系统，下载分别是

- ophub Armbian下载：https://github.com/ophub/amlogic-s9xxx-armbian/releases
- ophub openwrt下载：https://github.com/ophub/amlogic-s9xxx-openwrt/releases

我先是从 ophub 下载了一个最新版本的 armbian，两周以前的。先是看了看线刷的教程，没有搞定，用 type-C usb 的线，一头插在am40一头插在我的mac上面，然后 adb 之类的总是找不到设备。折腾后搞不定就放弃了，其实主要是手头没有 tf 卡，要不都懒得折腾。放弃后狗东下单了 tf 卡和读卡器，第二天快递到了继续折腾的。

然后就卡刷，启动，发现显示器无显示。怀疑怕是最新的镜像有什么不支持，于是又下载了一个老一点的版本的，[2024年11月的](https://github.com/ophub/amlogic-s9xxx-armbian/releases/tag/Armbian_HassIoSupervisor_bookworm_save_2024.11)，这样就跟前面那篇博客中用到的版本一样了。结果还是显示器无法显示。

从路由器查看机器的ip，然后ssh能连上去了，看起来除了显示有问题，其它一切正常。网上查了一下，应该是hdmi需要打补丁，见这里 [am40 RK3399 需要更新下dtb文件，使用基本完美](https://github.com/ophub/amlogic-s9xxx-armbian/issues/1617)。最后我是从[这里](https://github.com/ophub/amlogic-s9xxx-armbian/issues/1317#issuecomment-2644482535)下载了 am40.tar，里面有个 install.sh，然后解压安装。再重启后hdmi 就有显示了，不过显示也不算正常，4k屏幕只用到了部分区域。另外，这个补丁将 linux 内核版本降回到 5.11 去了，原来是 6.多的版本，并且使用上之后上面的 usb3 接口也有问题了。anyway，不管它了，反正我当服务器用也不需要彻底解决这个问题。

## 配置samba

如果有镜像可以选择，直接刷OMV(openmediavault)之类的系统，会方便得多。这也是为什么x86的可玩性更高，PVE之后随便刷些系统，互不干扰，也不担心玩坏。

没得选的时候，就只能armbian了手动配置这些。装个samba就行，直接把挂上去的硬盘变成家庭nas。
网上有很多的[教程](https://picockpit.com/raspberry-pi/zh/%E7%94%A8-samba-%E5%B0%86-raspberry-pi-%E5%8F%98%E6%88%90-nas/)可以参考的。

挂载ntfs是使用 -t ntfs3 而不是 ntfs，ntfs3 写入速度要快得多。ntfs-3g 最开始是在用户空间实现的，网上搜到一些老的内容使用这个就信息过时了。[这个内容](https://zhuanlan.zhihu.com/p/539570785)是对的。

这样挂载之后 ntfs 这边的磁盘速度已经不再是瓶颈，usb3 写入的理论速度大概 500MB/s，瓶颈是我挂的一个机械硬盘，7200转速的sata盘大概瓶颈就这样。

写入速度：

```
time dd if=/dev/zero of=largefile bs=4k count=200000
输入了 200000+0 块记录
输出了 200000+0 块记录
819200000 字节 (819 MB, 781 MiB) 已复制，5.03833 s，163 MB/s

real	0m5.043s
user	0m0.159s
sys	0m3.582s
```


读取速度：

```
dd if=largefile of=/dev/null bs=4k count=200000
输入了 200000+0 块记录
输出了 200000+0 块记录
819200000 字节 (819 MB, 781 MiB) 已复制，0.57836 s，1.4 GB/s
```

然后是开启 samba 服务之后的速度测试。自己把自己坑了一把。开始我在 mac 上面测试了一下读写速度，写入是 33MB/s，读取是 66MB/s。

```
time dd if=/dev/zero of=largefile bs=4k count=200000
200000+0 records in
200000+0 records out
819200000 bytes transferred in 24.709145 secs (33153717 bytes/sec)

time dd of=/dev/null if=largefile bs=4k count=200000
200000+0 records in
200000+0 records out
819200000 bytes transferred in 12.277643 secs (66722904 bytes/sec)
```

然后到了 linux 机器上再测试，发现读写才 2MB/s 这种速度...
于是折腾是不是我 samba 配置哪里搞得不对，改配置，重启，测试，搞了好多轮，总是有问题，而且有些轮还把 mac 那边的速度也搞坏了。最后才发现**这个坑在于"连接到服务器"这个使用习惯**，mac 那边访达 cmd+K 然后打开 `smb://` 这种协议就可以直接 mount 设备到本地，手机和 ubuntu 都有类似的功能，所以我都是这么用的。

但是被坑到的地方是，**ubuntu 上面文件管理器里面虽然可以打开这样的链接，但是它用的不是 cifs 协议**！我查看了一下 mount 后才注意到这样的问题：

> gvfsd-fuse on /run/user/1000/gvfs type fuse.gvfsd-fuse (rw,nosuid,nodev,relatime,user_id=1000,group_id=1000)

改成手动 mount 之后，samba 的读写速度就快了许多，分别到了 61MB/s 和 42.7MB/s：

> sudo mount //192.168.0.11/DATA DATA -t cifs -o username=samba,iocharset=utf8,password=xxx,uid=1000,gid=1000


```
time dd of=/dev/null if=largefile bs=4k count=20000
20000+0 records in
20000+0 records out
81920000 bytes (82 MB, 78 MiB) copied, 1.34264 s, 61.0 MB/s

real    0m1.354s
user    0m0.008s
sys     0m0.055s
time dd if=/dev/zero of=largefile bs=4k count=200000
200000+0 records in
200000+0 records out
819200000 bytes (819 MB, 781 MiB) copied, 19.1857 s, 42.7 MB/s

real    0m19.703s
user    0m0.033s
sys     0m1.229s
```

千兆网卡的理论传输速度大概 125MB/s，我这里没有达到，应该是瓶颈在 samba 那边，还是有什么地方没有最优化。[网上看到](https://zhuanlan.zhihu.com/p/305265719)一个地方说 samba 异步导致读取速度减半，建议设置 `aio read size = 0`，这个改掉之后 mac 这边大概 70MB/s 的读取速度，linux 那边是 77MB/s (似乎有缓存影响，多执行几次是越来越快的)。

```
time dd of=/dev/null if=largefile bs=4k count=200000
200000+0 records in
200000+0 records out
819200000 bytes transferred in 11.481162 secs (71351663 bytes/sec)
dd of=/dev/null if=largefile bs=4k count=200000  0.07s user 1.54s system 13% cpu 11.502 total

time dd of=/dev/null if=largefile bs=4k count=200000
200000+0 records in
200000+0 records out
819200000 bytes transferred in 1.432811 secs (571743238 bytes/sec)
dd of=/dev/null if=largefile bs=4k count=200000  0.04s user 0.67s system 48% cpu 1.448 total

time dd of=/dev/null if=largefile bs=4k count=200000
200000+0 records in
200000+0 records out
819200000 bytes transferred in 0.633795 secs (1292531497 bytes/sec)
dd of=/dev/null if=largefile bs=4k count=200000  0.04s user 0.60s system 97% cpu 0.653 total

time dd of=/dev/null if=largefile bs=4k count=200000
200000+0 records in
200000+0 records out
819200000 bytes transferred in 0.624890 secs (1310950727 bytes/sec)
dd of=/dev/null if=largefile bs=4k count=200000  0.04s user 0.59s system 98% cpu 0.643 total
```

(最后意识到瓶颈不是 samba 配置导致的，而是因为我用 wifi 而受到的影响，chatgpt 帮忙分析得出的结论)

## 配置旁路由

sing-box，配置很复杂，网上可以找到一个配置的[视频讲解](https://www.youtube.com/watch?v=oKvYpGo_kvw)。

服务端配置见[这篇](https://apad.pro/sing-box-trojan/)。

```
{
  "inbounds": [
    {
      "type": "trojan",
      "listen": "0.0.0.0",
      "listen_port": 3443,
      "tcp_fast_open": true,
      "udp_fragment": true,
      "users": [
        {
          "name": "xxxx",
          "password": "xxxxxxxx"
        }
      ],
      "tls": {
        "enabled": true,
        "server_name": "xxxxxxxxxxx.nip.io",
        "alpn": [
          "h2",
          "http/1.1"
        ],
        "min_version": "1.2",
        "max_version": "1.3",
        "cipher_suites": [
        "TLS_CHACHA20_POLY1305_SHA256",
        "TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256",
        "TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256"
        ],
        "acme": {
          "domain": ["xxx.xxx.xxx.xxx.nip.io"],
          "data_directory": "/usr/local/etc/sing-box",
          "email": "xxx.xxx.xxx.xxx@nip.io",
          "provider": "letsencrypt"
        }
      },
      "fallback": {
        "server": "127.0.0.1",
        "server_port": 8080
      }
    }
  ]
}
```

客户端配置如果是浏览器使用，这个已经够了:

```
{
    "inbounds": [
        {
            "type": "mixed",
            "listen": "::",
            "listen_port": 1180
        }
    ],
    "outbounds": [
        {
            "type": "trojan",
            "server": "xxx.xxx.xxx.xxx.nip.io",
            "server_port": 3443,
            "password": "xxxxxxx",
            "tls": {
                    "enabled": true
            }
        }
    ]
}
```

不过配置旁路由就复杂许多，[这里的配置](https://github.com/mario-huang/sing-box-bypass-router-transparent-proxy-configuration/blob/7b0c49b9337724c82b3d5d376c53ccebf0874549/README.md)可以参考。或者[这个](https://icloudnative.io/posts/sing-box-tutorial/)。
