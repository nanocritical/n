import errors
import ast
import re

class Scope(object):
  def __init__(self, container):
    self.container = container
    self.table = {}
    self.imports = []
    self.parent = None

  def _qimports(self, name):
    for i in xrange(len(self.imports)-1, -1, -1):
      t = self.imports[i].scope.table
      if name in t:
        return t[name]

  def q(self, name):
    if name in self.table:
      return self, self.table[name]
    elif self.parent is not None:
      return self.parent.q(name)
    else:
      raise errors.ScopeError("'%s' not found in scope '%s %s'" \
          % (name, self, self.container))

  def define(self, what, name=None):
    name = name or what.name
    if name in self.table or self._qimports(name) is not None:
      raise errors.ScopeError("In scope '%s %s', name '%s' already defined" \
          % (self, self.container, what))
    self.table[name] = what

    if 'scope' in what.__dict__:
      if what.scope is None:
        raise errors.ScopeError("In scope '%s %s', element named '%s' has a None scope" \
            % (self, self.container, what))
      elif what.scope.parent is not None:
        raise errors.ScopeError("In scope '%s %s', subscope '%s %s' already has a parent" \
            % (self, self.container, what.scope, what.scope.container))
      what.scope.parent = self

  def alias(self, name, module, orig):
    self.table[name] = module.scope.q(orig)

  def fullname(self, node):
    if node == self.container:
      name = ''
    else:
      name = Scope.CNAME_SEPARATOR + node.name

    if self.parent is None:
      return self.container.name + '.' + name
    else:
      return self.parent.fullname(self.container) + '.' + name

  CNAME_SEPARATOR = '_'
  def fullcname(self, node):
    if node == self.container:
      name = ''
    else:
      name = Scope.CNAME_SEPARATOR + node.name

    if self.parent is None:
      if isinstance(self.container, ast.Module):
        return re.sub(r'\.', Scope.CNAME_SEPARATOR, self.container.name) + name
      else:
        return self.container.name + name
    else:
      return self.parent.fullcname(self.container) + name
