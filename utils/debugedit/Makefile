debugedit: debugedit.c hashtab.c linemap.c ../../bootstrap/hash.c
	ccache gcc -g -std=c99 -Wall -O2 -o $@ $^ -lelf -lpopt

linemap: linemap.c ../../bootstrap/hash.c
	ccache gcc -DLINEMAP_STANDALONE -g -std=c99 -Wall -O2 -o $@ $^ \
	  && bash -c 'diff -u test.linemap <(./$@ test)'
