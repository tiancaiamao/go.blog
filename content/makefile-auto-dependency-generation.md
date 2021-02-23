makefile 的自动生成依赖

来源于 http://make.mad-scientist.net/papers/advanced-auto-dependency-generation/

记录一下关键信息。

```
DEPDIR := .deps
DEPFLAGS = -MT $@ -MMD -MP -MF $(DEPDIR)/$*.d

COMPILE.c = $(CC) $(DEPFLAGS) $(CFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c

%.o : %.c
%.o : %.c $(DEPDIR)/%.d | $(DEPDIR)
        $(COMPILE.c) $(OUTPUT_OPTION) $<

$(DEPDIR): ; @mkdir -p $@

DEPFILES := $(SRCS:%.c=$(DEPDIR)/%.d)
$(DEPFILES):
include $(wildcard $(DEPFILES))
```

DEPDIR 依赖文件的路径

DEPFLAGS 生成依赖使用的 flag，其中

-MT $@ 指定生成的依赖文件的文件名

-MMD 生成依赖信息，而不是执行编译。这个生成的依赖会忽略掉系统头文件，如果要带系统头文件，则使用 -MD

-MP 对列表中的每个前置条件，添加一个目标。这个标记可以避免删除文件后，make 出错

-MF `$(DEPDIR)/$*.d` 将生成的依赖文件写到 `$(DEPDIR)/$*.d`

%.o : %.c 覆盖掉系统默认的规则，因为我们会使用自己的规则

... $(DEPDIR)/%.d  目标文件会依赖生成的依赖文件，如果依赖文件不存在，它会先被构建出来

... | $(DEPDIR) 声明依赖文件夹也是目标文件的依赖，这样如果它不存在就会被自动创建

$(DEPDIR): ; @mkdir -p $@ 生成依赖文件夹

DEPFILES 依赖文件

$(DEPFILES): 这个空的规则使得，如果文件不存在也不会报错

include ... 引用依赖文件， wildcard 可以避免文件不存在时的失败
