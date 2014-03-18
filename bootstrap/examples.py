#!/usr/bin/env python2

import re
import sys
import subprocess

def sh(cmd):
    try:
        return subprocess.check_output(cmd, shell=True)
    except subprocess.CalledProcessError as ignore:
        return ''

SECTION = '.text.examples'

collision = set()

def get_examples(obj_fn):
    r = []
    p = re.compile(r'(?:\S+\s+){2}F\s+' + SECTION + r'(?:\.(_\w+))?\s+\S+\s+(\w+)')
    out = sh("objdump -t " + obj_fn + " 2> /dev/null")
    for ln in out.split('\n'):
        m = p.match(ln)
        if m is not None:
            kind, name = m.group(1), m.group(2)
            if name in collision:
                raise Exception("Two examples are named '%s'" % name)
            collision.add(name)
            r.append((kind, name))
    return r

order = { None: 0, '_NCC_EMPTY': 10, '_NCC': 20 }

def cmp_examples(a, b):
    pa = order[a[0]] if a[0] in order else 100
    pb = order[b[0]] if b[0] in order else 100
    return cmp(pa, pb)

def gen_main(out, examples):
    print>>out, '''#include "common.h"
'''
    examples.sort(cmp=cmp_examples)

    for kind, ex in examples:
        kind = kind or ''
        print>>out, '''
void %s(EXAMPLES_PROTO%s);''' % (ex, kind)

    print>>out, '''
int main(void) {'''

    for kind, ex in examples:
        kind = kind or ''
        print>>out, '''
    {
      EXAMPLES_DECLS%s;
      examples_init%s("%s" EXAMPLES_INIT_ARGS%s);
      %s(EXAMPLES_ARGS%s);
      examples_destroy%s("%s" EXAMPLES_INIT_ARGS%s);
    }''' % (kind,
            kind, ex, kind,
            ex, kind,
            kind, ex, kind)

    print>>out, '''
}'''

def main():
    examples = []
    out_fn = sys.argv[1]
    objects = sys.argv[2:]
    for obj_fn in objects:
        examples.extend(get_examples(obj_fn))

    examples.sort(key=(lambda a: a[0]))

    with open(out_fn, 'w') as out:
        gen_main(out, examples)

if __name__ == '__main__':
    main()
