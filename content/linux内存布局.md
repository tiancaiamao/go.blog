## 内存布局

栈
堆
数据段
代码段

从低地址往高地址依次是：代码段->数据段->堆->栈

mmap的地址空间是位于堆与栈之间的

## malloc的实现

malloc的实现是调用brk或mmap

若分配小于128K的内存，则调用brk

数据段最上面有个edata指针，brk会将edata指针往上推\\ 回收的时候是回收到malloc库的内存池，不会归还系统

brk分配的只是虚拟地址，当访问相应内存时会发生缺页中断分配物理地址

若分配的内存大于128K，则由mmap在栈和堆之间分配一块内存

## 测试

```
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>

int global;
int main() {
        int stack;
        void *heap;
        void *mmaped;
        int fd;

        fd = open("aaa", O_RDWR|O_CREAT|O_TRUNC);
        if(fd == -1) {
                perror("open");
                goto open_error;
        }
        if(lseek(fd, 23, SEEK_SET) == -1) {
                perror("lseek");
        if(lseek(fd, 23, SEEK_SET) == -1) {
                perror("lseek");
                goto lseek_error;
        }
        if(write(fd, "", 1) != 1) {
                perror("write");
                goto lseek_error;
        }
        heap = malloc(34);
        mmaped = mmap(NULL, 22, PROT_WRITE, MAP_SHARED, fd, 0);
        if(mmaped == MAP_FAILED) {
                perror("mmap error");
                goto wrong;
        }
        printf("data: %p\nstack: %p\ncode: %p\nheap: %p\nmmap: %p\n",
               &global,
               &stack,
               main,
               heap,
               mmaped);
        free(heap);
               mmaped);
        free(heap);
        return 0;
wrong:
        free(heap);
lseek_error:
        close(fd);
open_error:
        exit(-1);
}
```

写了个简单的代码测试了一下各个数据在内存区域的分布，运行结果如下：

    data: 0x804a040
    stack: 0xbfea2ddc
    code: 0x80485b4
    heap: 0x9c12008
    mmap: 0xb7748000
