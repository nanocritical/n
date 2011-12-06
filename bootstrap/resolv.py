import re
import os
import os.path
import errors

def _findfile(f):
  if os.path.isfile(f):
    return f
  elif os.path.isfile(os.path.join('nlang-site/', f)):
    return os.path.join('nlang-site/', f)
  else:
    return None

def _finddir(d):
  if os.path.isfile(os.path.join(d, 'module.n')):
    return os.path.join(d, 'module.n')
  elif os.path.isfile(os.path.join('nlang-site/', d, 'module.n')):
    return os.path.join('nlang-site/', d, 'module.n')
  else:
    return None

def find(modname):
  base = re.sub(r'\.', '/', modname)
  file = base + '.n'
  dir = base

  f = _findfile(file)
  if f is not None:
    return f

  d = _finddir(dir)
  if d is not None:
    return d

  raise errors.Error("Module '%s' not found" % modname)

def fn2modname(fn):
  fn = re.sub(r'\.n$', '', fn)
  return re.sub(r'/', '.', fn)
