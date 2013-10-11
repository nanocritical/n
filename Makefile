MAKEFLAGS := -j2 -r -R --warn-undefined-variables
.SUFFIXES:

V ?=
Q = $(if $V,,@)

default:: ncc0

SRC = bootstrap
DEPS = .deps

CFLAGS += -std=c99 -Wall -O0 -g \
	  -Wmissing-prototypes -Wpointer-arith -Wstrict-prototypes \
	  -Wmissing-declarations -Wno-format-zero-length -Wbad-function-cast \
	  -Wcast-align -Wwrite-strings -Wno-missing-braces

deps-dir-for-target = $(dir $(DEPS)/$1)
deps-options = -MMD -MF $(DEPS)/$2.d -MT $1

re2c-sources := $(shell find $(SRC) -name \*.re)
re2c-outs := $(patsubst %.re,%.generated.c,$(re2c-sources))
.SECONDARY: $(re2c-outs)

%.generated.c: %.re
	$Qre2c -b -o $@ $<

sources := $(shell find $(SRC) -name \*.c -a ! -name \*.generated.\*)
sources += $(re2c-outs)

objects := $(patsubst %.c,%.o,$(sources))

%.o: %.c $(DEPS) $(deps-dir-for-target)
	$Qmkdir -p $(call deps-dir-for-target,$@)
	$Qgcc -c $(CFLAGS) $(call deps-options,$@,$<) -o $@ $<

ncc0: $(objects)
	$Qgcc -g $(CFLAGS) -o $@ $^

$(DEPS)/ $(DEPS):
	$Qmkdir -p $(DEPS)

include $(shell find $(DEPS) -name \*.d 2> /dev/null)
