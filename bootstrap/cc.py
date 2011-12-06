import os
import re
import sys
import hashlib
import subprocess

import parser
import errors
import cwriter
import resolv


class Options:
  def __init__(self):
    self.sources = []
    self.output = 'a.out'
    self.nlangdir = '/home/e/nc/n/nlang-site'


def objectfn(fn):
  return re.sub('\.n$', '.o', fn)

def cfn(fn):
  return re.sub('\.n$', '.gen.c', fn)

gobjectcache = {}

def compile(opt, fn):
  modname = resolv.fn2modname(fn)

  global gobjectcache
  if modname in gobjectcache:
    return 0, gobjectcache[modname]
  else:
    c = cfn(fn)
    o = objectfn(fn)

    mod = parser.parsemod(modname)
    with open(c, 'w') as out:
      mod.cwrite(out)

    p = subprocess.Popen(['gcc', '-pipe', '-DNLANG_BOOTSTRAP',
      '-I', opt.nlangdir, '-o', o, '-std=c99', '-xc', '-c', c])
    p.wait()
    if p.returncode == 0:
      gobjectcache[modname] = o
    return p.returncode, o

def link(opt, ofns):
  return subprocess.call(['gcc', '-pipe', '-o', opt.output] + ofns)


def run(opt):
  ofns = []
  for fn in opt.sources:
    ret, ofn = compile(opt, fn)
    if ret != 0:
      return ret
    ofns.append(ofn)

  return link(opt, ofns)


def runcmdline(args):
  opt = Options()
  for i in range(len(args)):
    arg = args[i]
    val = None
    if i < len(args)-1:
      val = args[i+1]

    if arg[0] != '-':
      opt.sources.append(arg)
    else:
      name = arg[1:]
      if name not in opt.__dict__:
        raise errors.Error("Option '%s' is unknown" % arg)

      if val is None:
        setattr(opt, name, True)
      else:
        setattr(opt, name, val)

  if len(opt.sources) == 0:
    raise errors.Error("No input specified")

  return run(opt)
