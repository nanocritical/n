MAKEFLAGS := -j4 -r -R --warn-undefined-variables
.SUFFIXES:

V ?=
Q = $(if $V,,@)
O ?= 0
P ?=
I ?=
S ?=
SKIP_EXAMPLES ?=

default:: ncc0 $(if $(SKIP_EXAMPLES),,examples.run)

SRC = bootstrap
DEPS = .deps
CC ?= gcc

ifneq ($(shell ccache --version 2> /dev/null),)
	override CC := ccache $(CC)
endif

# Because of a GCC bug that sometimes fails to interpret stored cpp output
# differently. See 6e2acd44a444a95d2f2bc9f8e5dfaae21559db79 in n.git.
export CCACHE_RUN_SECOND_CPP

override CFLAGS += -std=c99 -Wall -pthread -O$(O) -ggdb $(if $P,-pg,) \
	  -iquote. \
	  -Wmissing-prototypes -Wpointer-arith \
	  -Wmissing-declarations -Wno-format-zero-length -Wbad-function-cast \
	  -Wcast-align -Wwrite-strings -Wno-missing-braces -Wstrict-prototypes \
	  -Wmaybe-uninitialized -Wuninitialized $(if $I,-DINVARIANTS=$I,) \
	  $(if $S,-DCONFIG_STATIT=$S,)

deps-dir-for-target = $(dir $(DEPS)/$1)
deps-options = -MMD -MF $(DEPS)/$2.d -MT $1
objects-for-sources = $(patsubst %.c,%.o,$1)

re2c-sources := $(shell find $(SRC) -name \*.re)
re2c-outs := $(patsubst %.re,%.generated.c,$(re2c-sources))
.SECONDARY: $(re2c-outs)

sources := $(shell find $(SRC) -name \*.c -a ! -name \*.generated.\* -a ! -name \*.main.c)
sources += $(re2c-outs)

examples-py := bootstrap/examples.py
examples-generated-sources := bootstrap/examples.generated.c

$(examples-generated-sources): $(call objects-for-sources,$(sources)) $(examples-py)
	$Q$(examples-py) $@ $(filter-out $(examples-py),$^)

examples-sources := $(sources)
examples: $(call objects-for-sources,$(examples-sources)) $(examples-generated-sources)
	$Q$(CC) -g $(CFLAGS) -o $@ $^

examples.run: examples
	$Q./$< && touch examples.run

ncc0-sources := bootstrap/ncc0.main.c $(sources)
ncc0: $(call objects-for-sources,$(ncc0-sources))
	$Q$(CC) -g $(CFLAGS) -o $@ $^

$(DEPS)/ $(DEPS):
	$Qmkdir -p $(DEPS)

include $(shell find $(DEPS) -name \*.d 2> /dev/null)


%.generated.c: %.re
	$Qre2c -b -o $@ $<

%.o: %.c $(DEPS)
	$Qmkdir -p $(call deps-dir-for-target,$@)
	$Q$(CC) -c $(CFLAGS) $(call deps-options,$@,$<) -o $@ $<
