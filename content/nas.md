刚过 618 活动，买了个 2T 的 SSD 盘，升级下设备。还是之前的[smart-am40](/smart-am40.md)用来做 NAS。在此之前我是 usb3 外接的移动硬盘盒，盒子上插了个旧的 hdd 盘。那块盘可能有十多年了，最早是买了个 macbook air，存储空间不够，于是买了个 1T 的移动硬盘，后来 macbook air 退役了，移动硬盘也有点接触不良。再后来干脆把里面的硬盘拆了下来，又买了个硬盘盒，把拆下来的硬盘装上去。然后外接 nas 继续服役。

这个移动硬盘盒有两个盘位，sata 的接口。我想着还能扩展一下，于是买了这个 2T 的 SSD。其实主要是几块考虑：

- 存储容量再扩一扩
- 可靠性增强下，别丢数据
- 最好性能还往上提一点

ssd 更抗震一些，性能也更好，然后家用没那么多的反复读些，寿命不是问题，所以几乎全方位是优于 hdd。hdd 除了容量有点优势，其它基本都不如 ssd，可以无脑选 ssd。

新的盘到了之后，我研究了一下怎么分配空间。旧的盘已经有一些坏道了，用了那么久了，不太放心，所以我决定拿它当 backup 用。用 btrfs 文件系统，这样分配：

- data 子卷：主数据，位于 2T SSD 上；
- snapshot 子卷：在同一块 SSD 上，保留多个时间点快照；
- backup：位于 1T HDD 上，通过 btrfs send 定期备份 snapshot。

也考虑过 RAID，比如 RAID1 或者 RAID10，不过现在我已经是有数据的场景，不好从头折腾。另外是据说 btrfs 组软件 RAID 稳定性还是有些问题。现在这样其实就是最适合的场景，data 和 backup 在不同的盘上面，只要不是一起坏还是有一定的安全保证的。新的主要存储放在 ssd 这个会比之前的性能提升一些。

计划的升级流程是这样，先把 ssd 初始化 btrfs 文件系统；然后把旧的数据从以前的 ntfs 拷过去；拷完之后旧的 hdd 就可以直接格式化了重建 btrfs 系统。一些操作流程记录一下：

初始化盘

```
mkfs.btrfs /dev/sdb  # 主盘 ssd
mkfs.btrfs /dev/sdc1  # 备份盘 hdd
```

使用 Btrfs 子卷 + 快照管理

```
btrfs subvolume create /mnt/ssd/data
btrfs subvolume create /mnt/ssd/snapshot
```

重要子卷做快照

```
btrfs subvolume create /mnt/ssd/data/important
btrfs subvolume snapshot -r /mnt/ssd/data/important /mnt/ssd/snapshots/important-20250620
```

首次备份

```
btrfs send /mnt/snapshot/important-20250620 | btrfs receive /mnt/backup/
```

后期打快照和增量备份

```
btrfs subvolume snapshot -r /mnt/ssd/data/important /mnt/ssd/snapshots/important-202506xx
btrfs send -p /mnt/snapshot/important-202506xx | btrfs receive /mnt/backup/
```

目录结构：

```
/mnt/ssd/data                ← 根子卷
├── important            ← 子卷 (重要数据快照后备份)
│   ├── photos           ← 
│   ├── docs             ← 
│   └── temp             ← 
├── downloads            ← 普通目录，不快照
└── cache                ← 普通目录，不快照

/mnt/snapshot/
└── important-2025-06-19

/mnt/backup/
└── important-2025-06-19
```

拷文件的时候发现拷得特别慢，于是测试了一下速度，发现新的 SSD 的读写只有 170MB/s，这严重不符合预期。商品描述应该是 480-500MB/s 的速度，于是研究咋回事。usb3.0 不应该是瓶颈，它的速度至少 500MB/s 级别，SATA 接口不应该是瓶颈，而 ssd 即便买到水货也不可能水到这种程度，这水到要退货...最后还是跟着 chatgpt 研究了一圈，发现根因是 UAS。

usb3.0 外接sata盘这种形式，如果硬件支持，它应该工作在 uas 模式，而如果硬件不支持，则回退到 BULK only 模式，lsusb -t 如果看到 Driver=usb-storage，则说明没有工作在 uas 模式，性能就会受影响发挥不出来。我查了一下我的硬件情况，Orico的一个2.5/3.5硬盘盒，商品描述上是支持 usa 协议的。所以问题就到了我的 linux 软件驱动层。我发现是对应的 armbian 内核编译没有把 uas 模块编译进去，于是我去给作者提了个 [issue](https://github.com/ophub/amlogic-s9xxx-armbian/issues/3002)。

为了确认我的 ssd 没问题，我又换到我的台式机上再测试一下，在 ubuntu 系统里面 uas 内核模块是自带了的。结果发现还有其它问题，这个硬盘盒的芯片兼容性不好，于是内核针对这个硬件把 uas 禁用掉了。dmesg 可以看到这样的信息:

```
usb-storage 4-1:1.0: Quirks match for vid 152d pid 0567: 1800000
```

把对话从 chatgpt 那边粘过来：

> JMicron JMS567 桥接芯片与 Linux UASP 兼容性问题
> 你的硬盘盒使用的是 JMicron JMS567 (根据 VID 152d 和 PID 0567 推断，这是 JMicron JMS567 芯片的常见 ID)。
> 
> 问题出在这里：JMicron JMS567 芯片在 Linux 系统下的 UASP 兼容性是臭名昭著的。
> 
> 背景： 许多 JMicron 芯片（包括 JMS567）在 UASP 模式下与 Linux 内核存在已知的问题。它们在 Windows 或 macOS 下可能表现良好，但在 Linux 下，UASP 模式下可能会导致 数据损坏、连接不稳定、设备突然断开连接 等严重问题。
> 内核的应对： 为了防止这些问题，Linux 内核维护者主动为 JMicron 的这些问题芯片添加了 "quirks" (怪异) 或 "blacklist" (黑名单) 规则。这些规则告诉内核：当检测到这些特定的 VID:PID 组合时，即使设备可能支持 UASP，也不要使用 uas 驱动，而是强制回退到更稳定但慢速的 usb-storage (BOT) 驱动。
> 你的日志证实了这一点： Quirks match for vid 152d pid 0567: 1800000 这条日志就是内核在明确告诉你：“我知道这个设备（JMicron JMS567），而且我知道它在 UASP 模式下有问题，所以我决定强制它使用 usb-storage。”
> 

好吧，只能先用着，自我安慰下：反正哪怕 ssd 这边的速度能发挥出来，网络那一层也会成为最终瓶颈。千兆网络那边 125MB/s 会是上限，我之前的瓶颈实际是在 wifi 这边，只能 70MB/s 的样子。这个 170MB/s 的 ssd 也就这样了吧。
