import os
import re
import sys
import hashlib


class Error(Exception):
  pass


class Type(object):
  def __init__(self, name):
    self.name = name
  def __str__(self):
    return self.name
  def __hash__(self):
    return hash(self.name)

class Typedecl(object):
  def __init__(self, name):
    self.name = name
  def __str__(self):
    return self.name
  def __hash__(self):
    return hash(self.name)

class Typedef(Typedecl):
  def __init__(self, name, aliasing):
    self.aliasing = aliasing
    super(Typedef, self).__init__(name)

class Record(Typedecl):
  def __init__(self, name):
    self.fields = {}
    super(Record, self).__init__(name)

class Enum(Typedecl):
  pass

class Taggedunion(Typedecl):
  pass

class Union(Typedecl):
  pass

class Template(Typedecl):
  pass

class Function(Typedecl):
  pass


xtypename = r'[A-Z]\w*'
_xtypeinst = xtypename + r'\w+'
xtypeinst = r'(\(' + _xtypeinst + r'\)|' + xtypename + r')'
xtypeconstr = r'\s+(\w+)\s*:\s*' + xtypeinst
re_typedecl = re.compile(r'type\s+(\w+)\s*:\s*')



class Options:
  self __init__(self):
    self.sources = []
    self.c = False


def run(opt):
  pass


def runcmdline(args):
  opt = Options()
  for i in range(len(args)):
    arg = sys.argv[i]
    val = None
    if i < len(sys.argv)-1:
      val = sys.argv[i+1]

    if arg[0] != '-':
      opt.sources.append(arg)
    else:
      name = arg[1:]
      if name not in opt.__dict__:
        raise Error("Option '%s' is unknown" % arg)

      if val is None:
        setattr(opt, name, True)
      else:
        setattr(opt, name, val)

    if len(opt.sources) == 0:
      raise Error("No input specified")

  return run(opt)
