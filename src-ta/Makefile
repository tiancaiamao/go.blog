# Build the md4c ta C module: src-ta/md4c/{glue.c, md4c.c} -> lib/md4c.dylib
#
# The ta runtime lazy-loads lib/md4c.dylib on first `md4c.<fn>` call
# (see tinyactor docs/c-module.md); codegen detects the dylib in lib/ to
# emit the lazy-load call, so this file must exist before compiling any
# .ta that calls md4c.events.
#
# Usage (from the go.blog repo root):
#   make -C src-ta md4c

TA_ROOT ?= /Users/genius/project/tinyactor

CC ?= cc
# -undefined dynamic_lookup: the VM symbols (val_*, tls_current_proc, ...)
# are resolved at dlopen time from the tavm binary (same as tinyactor's
# own lib/http.dylib build rule).
ifeq ($(shell uname -s),Darwin)
  MOD_EXT = dylib
  UNDEF_OK = -undefined dynamic_lookup
else
  MOD_EXT = so
endif
CFLAGS ?= -O2 -fPIC -shared -I. -I$(TA_ROOT) -Imd4c $(UNDEF_OK)
LIBDIR  := ../lib

.PHONY: all clean
all: md4c

md4c: ../lib/md4c.$(MOD_EXT)
	@echo "built $<"

../lib/md4c.$(MOD_EXT): md4c/glue.c md4c/md4c.c md4c/md4c.h $(TA_ROOT)/ta.h
	$(CC) $(CFLAGS) -o $@ md4c/glue.c md4c/md4c.c

clean:
	rm -f $(LIBDIR)/md4c.$(MOD_EXT)