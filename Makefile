MAKEFLAGS := -j2 -r -R
.SUFFIXES:

default:: ncc0

CFLAGS += -std=c99 -Wall -O0 -g \
	  -Wmissing-prototypes -Wpointer-arith -Wstrict-prototypes \
	  -Wmissing-declarations -Wno-format-zero-length -Wbad-function-cast \
	  -Wcast-align -Wwrite-strings

sources := \
	bootstrap/0/ncc0.c \
	bootstrap/0/common.c \
	bootstrap/0/lexer.c \
	bootstrap/0/parser0.c \
	bootstrap/0/firstpass0.c \
	bootstrap/0/printer0.c \
	bootstrap/0/printer_c0.c \
	bootstrap/0/hash.c \
	bootstrap/0/bitops.c

objects := $(patsubst %.c,%.o,$(sources))

%.c: %.re
	@re2c -b -o $@ $^

%.o: %.c
	@gcc -c $(CFLAGS) -o $@ $^

ncc0: $(objects)
	@gcc -g $(CFLAGS) -o $@ $^
