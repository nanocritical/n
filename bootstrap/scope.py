import re
import errors
import ast

builtintypes = set('''Void U8 I8 U16 I16 U32 I32 U64 I64 Char Size SSize'''.split())

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
    self.imports = []
    self.parent = None

  def _q_imports(self, name):
    for i in xrange(len(self.imports)-1, -1, -1):
      t = self.imports[i].scope.table
      if name in t:
        return t[name]

  def define(self, what, name=None):
    name = name or what.name
    if name in self.table or self._q_imports(name) is not None:
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

  def _q_field(self, node):
    what = self.table[node.name]

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
    if not isinstance(node, ast.TypeGenericArg):
      return node
    return node.instantiated

  def _q(self, node):
    if isinstance(node, ast.ValueField):
      return self._q_field(node)

    if node.name in self.table:
      return self.table[node.name]
    elif self.parent is not None:
      return self.parent.q(node)
    else:
      raise errors.ScopeError("'%s' not found in scope '%s %s'" \
          % (node.name, self, self.container))

  def q(self, node):
    return self._q_genarg(self._q(node))

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

    if not skipfield and isinstance(node, ast.ValueField):
      gen = self._q_field(node)
      if isinstance(gen, ast.TypeGenericArg):
        return gen.instantiated
      else:
        return self._fullcname_field(node)

    gen = self._q(node)
    if isinstance(gen, ast.TypeGenericArg):
      return gen.instantiated

    if node.name not in self.table:
      if self.parent is not None:
        return self.parent.fullcname(node)
      else:
        raise errors.ScopeError("'%s' not found in scope '%s %s'" \
            % (node.name, self, self.container))

    if isinstance(self.container, ast.Module):
      return re.sub(r'\.', Scope.CSEP, self.container.name) \
          + Scope.CSEP + node.name
    elif isinstance(node, ast.CGlobalName):
      return self.parent.fullcname(self.container) + Scope.CSEP + node.name
    else:
      return node.name
