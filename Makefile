MAKEFLAGS := -j2 -r -R
.SUFFIXES:

default:: ncc0

CFLAGS += -std=c99 -Wall -O0 -g \
	  -Wmissing-prototypes -Wpointer-arith -Wstrict-prototypes \
	  -Wmissing-declarations -Wno-format-zero-length -Wbad-function-cast \
	  -Wcast-align -Wwrite-strings

sources := \
	bootstrap/ncc0.c \
	bootstrap/common.c \
	bootstrap/lexer.c \
	bootstrap/parser.c \
	bootstrap/passes.c \
	bootstrap/printer.c \
	bootstrap/cprinter.c \
	bootstrap/hash.c \
	bootstrap/bitops.c

objects := $(patsubst %.c,%.o,$(sources))

%.c: %.re
	@re2c -b -o $@ $^

%.o: %.c
	@gcc -c $(CFLAGS) -o $@ $^

ncc0: $(objects)
	@gcc -g $(CFLAGS) -o $@ $^
