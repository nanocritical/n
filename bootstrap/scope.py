import re
import errors
import ast

builtintypes = set('''Void U8 I8 U16 I16 U32 I32 U64 I64 Char Size SSize Bool'''.split())

gscope = []

def down(scope):
  global gscope
  gscope.append(scope)

def up():
  global gscope
  assert len(gscope) > 0
  gscope.pop()

def current():
  global gscope
  assert len(gscope) > 0
  return gscope[-1]

def scopedname(node):
  global gscope
  return gscope[-1].fullcname(node)

class Scope(object):
  def __init__(self, container):
    self.container = container
    self.table = {}
    self.parent = None

    self.gendecls = []

  def _define(self, what, name=None):
    if 'name' in what.__dict__:
      name = name or what.name
      if name in self.table:
        raise errors.ScopeError("In scope '%s %s', name '%s' already defined, at %s" \
            % (self, self.container, what, what.codeloc))
      self.table[name] = what

    if 'scope' in what.__dict__:
      if what.scope is None:
        raise errors.ScopeError("In scope '%s %s', element named '%s' has a None scope, at %s" \
            % (self, self.container, what, what.codeloc))
      elif what.scope.parent is not None:
        raise errors.ScopeError("In scope '%s %s', subscope '%s %s' already has a parent, at %s" \
            % (self, self.container, what.scope, what.scope.container, what.codeloc))
      what.scope.parent = self

  def define(self, what, name=None):
    if isinstance(what, ast.Module):
      name = name or what.name
      path = name.split('.')
      scope = self
      prev = None
      for i in xrange(len(path)):
        p = path[i]
        if p in scope.table:
          existing = scope.table[p]
          if i == len(path) - 1:
            if isinstance(existing, ast.PlaceholderModule):
              del scope.table[p]
              scope._define(what, name=p)
              what.scope.table.update(existing.scope)
            else:
              raise errors.ScopeError("In scope '%s %s', name '%s' already defined, at %s" \
                  % (scope, scope.container, what, what.codeloc))
          else:
            pass
        else:
          if i == len(path) - 1:
            scope._define(what, name=p)
          else:
            scope.define(ast.PlaceholderModule(p))

        prev = scope.table[p]
        scope = scope.table[p].scope
    else:
      self._define(what, name=name)

  def _q_field(self, node):
    what = self.q(node, ignorefield=True)

    if isinstance(what, ast.VarDecl):
      what = what.typecheck()
    elif isinstance(what, ast.FieldDecl):
      what = what.type

    while True:
      if isinstance(what, ast.TypeApp):
        what = what.typeappdecl.typedecl
      elif isinstance(what, ast.TypeRef):
        what = what.deref(node.access)
      else:
        break

    if isinstance(node.field, basestring):
      return what.scope.table[node.field]
    else:
      return what.scope._q_field(node.field)

  def _q_genarg(self, node):
    if not isinstance(node, ast.GenericArg):
      return node
    return node.instantiated

  def rawq(self, node, ignorefield=False):
    if not ignorefield and isinstance(node, ast.ValueField):
      return self._q_field(node)

    if node.name in self.table:
      return self.table[node.name]
    elif self.parent is not None:
      return self.parent.q(node, ignorefield=ignorefield)
    else:
      raise errors.ScopeError("'%s' not found in scope %s, at %s" \
          % (node.name, self.container.codeloc, node.codeloc))

  def q(self, node, ignorefield=False):
    return self._q_genarg(self.rawq(node, ignorefield=ignorefield))

  def _fullcname_field(self, node):
    what = self.table[node.name]

    if isinstance(what, ast.VarDecl):
      what = what.typecheck()
    elif isinstance(what, ast.FieldDecl):
      what = what.type

    acc = '.'
    while True:
      if isinstance(what, ast.TypeApp):
        what = what.typeappdecl.typedecl
      elif isinstance(what, ast.TypeRef):
        what = what.deref(node.access)
        acc = '->'
      else:
        break

    if isinstance(node.field, basestring):
      return self.fullcname(node, skipfield=True) \
          + acc + node.field
    else:
      return self.fullcname(node, skipfield=True) \
          + acc + what.scope._fullcname_field(node.field)

  CSEP = '_'

  def fullcname(self, node, skipfield=False):
    if (isinstance(node, ast.FunctionDecl) \
        or isinstance(node, ast.MethodDecl)) \
        and node.name == 'main':
      return 'main'
    elif isinstance(node, ast.Type) and node.name in builtintypes:
      return node.name
    elif isinstance(node, ast.ExprSizeof):
      return 'sizeof'

    if not skipfield and isinstance(node, ast.ValueField):
      gen = self._q_field(node)
      if isinstance(gen, ast.GenericArg):
        return gen.instantiated
      else:
        return self._fullcname_field(node)

    gen = self.rawq(node)
    if isinstance(gen, ast.GenericArg):
      return gen.instantiated

    if node.name not in self.table:
      if self.parent is not None:
        return self.parent.fullcname(node)
      else:
        raise errors.ScopeError("'%s' not found in scope %s, at %s" \
            % (node.name, self.container.codeloc, node.codeloc))

    if isinstance(self.container, ast.Module):
      return re.sub(r'\.', Scope.CSEP, self.container.name) \
          + Scope.CSEP + node.name
    elif isinstance(node, ast.CGlobalName):
      return self.parent.fullcname(self.container) + Scope.CSEP + node.name
    else:
      return node.name
