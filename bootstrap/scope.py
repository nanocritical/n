import re
import errors
import ast
import typing
import copy

builtintypes = set(['<root>.nlang.numbers.' + n for n in '''void u8 i8 u16 i16 u32 i32 u64 i64 size ssize bool'''.split()])

gscope = []

def push(scope):
  global gscope
  if scope is not None:
    gscope.append(scope)
  return _ScopeCtxManager(scope is None)

def _pop():
  global gscope
  assert len(gscope) > 0
  gscope.pop()

class _ScopeCtxManager(object):
  def __init__(self, noop):
    self.noop = noop

  def __enter__(self):
    pass

  def __exit__(self, *args):
    if not self.noop:
      _pop()

def current():
  global gscope
  assert len(gscope) > 0
  return gscope[-1]

def _above():
  return _AboveCtxManager()

class _AboveCtxManager(object):
  def __enter__(self):
    global gscope
    assert len(gscope) > 1
    self.saved = gscope.pop()
  def __exit__(self, *args):
    global gscope
    gscope.append(self.saved)

def globalname(node):
  if isinstance(node, typing.Type):
    return node.cglobalname()
  elif isinstance(node, typing.Typename):
    return node

  if isinstance(node, ast.Expr):
    d = current().q(node)
  else:
    d = node

  if isinstance(d, ast.FunctionDecl) and node.name == 'main':
    return 'main'
  elif isinstance(node, ast.ExprSizeof):
    return 'sizeof'
  else:
    assert hasattr(d, 'scope')
    if d.scope.parent_definition is None:
      return d.scope.cglobalname()
    else:
      return globalname(d.scope.parent_definition.container) + '_' + d.name

def root(sc):
  if sc.parent is None:
    return sc
  else:
    return root(sc.parent)

class Scope(object):
  def __init__(self, container):
    self.container = container
    self.table = {}
    self.parent = None
    self.parent_definition = None

  def __str__(self):
    if hasattr(self.container, 'name'):
      cname = self.container.name
    else:
      cname = '<' + self.container.__class__.__name__ + '>'
    if self.parent is None:
      return cname
    else:
      p = self.parent_definition or self.parent
      return str(p) + '.' + cname

  def has_instantiable(self):
    return True

  def instantiated_copy(self, genenv, memo):
    cpy = copy.copy(self)
    memo[id(self)] = cpy
    new = False
    cpy.table = {}
    for k,v in self.table.iteritems():
      vcpy, vnew = ast._instantiated_copy(v, genenv, memo)
      cpy.table[k] = vcpy
      new = new or vnew
    try:
      cpy.parent = memo[id(self.parent)]
    except KeyError:
      pass
    try:
      cpy.parent_definition = memo[id(self.parent_definition)]
    except KeyError:
      pass
    cpy.container = memo[id(self.container)]
    return cpy, new

  def cglobalname(self):
    assert hasattr(self.container, 'name')
    cname = self.container.name
    if self.parent is None:
      return cname
    else:
      if self.parent._isroot():
        return cname
      else:
        return self.parent.cglobalname() + '_' + cname

  def __eq__(self, other):
    return str(self) == str(other)

  def __ne__(self, other):
    return not (self == other)

  def __lt__(self, other):
    return str(self).startswith(str(other))

  def __gt__(self, other):
    raise Exception()

  def contextid(self):
    return str(self)

  def _define(self, what, name=None, noparent=False):
    if hasattr(what, 'name'):
      name = name or what.name
      if name in self.table and what is not self.table[name]:
        raise errors.ScopeError(self, "Name '%s' already defined in '%s', at %s" \
            % (name, self, what.codeloc))
      self.table[name] = what

    elif isinstance(what, ast.PatternDecl):
      assert name is None
      for v in what.vars:
        self._define(v, noparent=noparent)

    if not noparent and hasattr(what, 'scope'):
      if what.scope is None:
        raise errors.ScopeError(self, "Element named '%s' has a None scope, at %s" \
            % (what, what.codeloc))
      elif what.scope.parent is not None:
        raise errors.ScopeError(self, "Subscope '%s' already has a parent '%s', at %s" \
            % (what.scope, what.scope.parent_definition or what.scope.parent, what.codeloc))
      what.scope.parent = self
      what.scope.parent_definition = self

  def define(self, what, name=None, noparent=False):
    if isinstance(what, ast.Module):
      path = what.path
      scope = root(self)
      prev = None
      for i in xrange(len(path)):
        p = path[i]
        if p in scope.table:
          existing = scope.table[p]
          if i == len(path) - 1:
            if isinstance(existing, ast.PlaceholderModule):
              del scope.table[p]
              scope._define(what, name=p, noparent=True)

              table_copy = existing.scope.table.copy()
              for n, d in table_copy.iteritems():
                if not isinstance(d, ast.PlaceholderModule):
                  # FIXME: before deleteing the old content (put there by 'declare'),
                  # check that it was consistent with the new.
                  del existing.scope.table[n]

              what.scope.table.update(existing.scope.table)
            elif isinstance(existing, ast.Module):
              scope = existing.scope
            else:
              raise errors.ScopeError(self, "name '%s' already defined in '%s', at %s" \
                  % (what, scope, what.codeloc))
          else:
            pass
        else:
          if i == len(path) - 1:
            scope._define(what, name=p, noparent=noparent)
          else:
            scope.define(ast.PlaceholderModule(path[0:i+1]))

        if i < len(path) - 1:
          prev = scope.table[p]
          scope = scope.table[p].scope
    else:
      self._define(what, name=name, noparent=noparent)

  def _qfield(self, node):
    cont = node.container.typecheck()
    while True:
      if cont.is_some_ref():
        if node.container.is_meta_type():
          what = cont.concrete_definition()
          break

        cont = cont.deref(node.access)
        continue
      else:
        what = cont.concrete_definition()
        break

    if isinstance(node.field, basestring):
      field = node.field
    else:
      field = node.field.name

    if isinstance(node.container, ast.ExprField) \
        and node.container.container is not None \
        and what == node.container.container.concrete_definition():
      # Hack to handle the EnumType.FIELD.value case:
      # EnumType.FIELD and EnumType.FIELD.value have the same type and
      # concrete_definition, so we need to special case it here.
      what = what.scope.q(ast._QueryWrapper(node.container.field.name))

    if isinstance(what, ast.FieldStaticConstDecl):
      what = what.scope.parent.container

    if field in what.scope.table:
      return what.scope.table[field]
    else:
      raise errors.ScopeError(what.scope, "'%s' not found, at %s" \
          % (field, node.codeloc))

  def _qgenarg_rec(self, node):
    global gscope
    with _above():
      r = current().q(node)
      if r is not None:
        return r
      elif len(gscope) <= 1:
        return None
      else:
        return self._qgenarg_rec(node)

  def _qgenarg(self, scopefound, node, queriednode):
    return node
    if node is None:
      return None
    elif not isinstance(node, ast.GenericArg):
      return node
    else:
      return self._qgenarg_rec(node)

  def _isroot(self):
    return isinstance(self.container, ast.PlaceholderModule) \
        and self.container.name == '<root>'

  def rawq(self, node, innerscope=None, ignorefield=False):
    innerscope = innerscope or self

    if self._isroot():
      if hasattr(node, 'name') and node.name == '<root>':
        return self.container

    if self.parent is not None and isinstance(node, typing.Type):
      return root(self).rawq(node.asexpr(), innerscope=innerscope, ignorefield=ignorefield)

    if not ignorefield and isinstance(node, ast.ExprField):
      return self._qfield(node)
    if not ignorefield and (isinstance(node, ast.ExprRef) \
        or isinstance(node, ast.ExprMutableRef)):
      return self.rawq(node.value, innerscope=innerscope, ignorefield=ignorefield)
    if not ignorefield and isinstance(node, ast.ExprCall):
      return self.q(node.args[0], innerscope=innerscope, ignorefield=ignorefield)

    if node.name in self.table:
      return self.table[node.name]
    elif self.parent is not None:
      return self.parent.q(node, innerscope=innerscope, ignorefield=ignorefield)
    else:
      raise errors.ScopeError(innerscope, "'%s' not found, at %s" \
          % (node.name, node.codeloc))

  def q(self, node, **kw):
    return self._qgenarg(self, self.rawq(node, **kw), node)
