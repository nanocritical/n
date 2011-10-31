import re
import os
import os.path
import errors

def find(modname):
  path = re.sub(r'\.', '/', modname) + '.n'
  if os.path.exists(path):
    return path
  elif os.path.exists('nlang/' + path):
    return 'nlang/' + path
  else:
    raise errors.Error("Module '%s' not found" % modname)

def fn2modname(fn):
  fn = re.sub(r'\.n$', '', fn)
  return re.sub(r'/', '.', fn)
