import re
import errors
import scope
import ast

def qbuiltin(name):
  x = ast.path_as_expr(name)
  return scope.current().q(x).typecheck()

class Typename(object):
  def __init__(self, name):
    self.name = Typename.normalize(name)
    self.codeloc = ast.CodeLoc(self)

  @staticmethod
  def normalize(name):
    return re.sub(r'\s*,\s+', ', ', re.sub(r'\s+', ' ', name))

  def __str__(self):
    return self.name

  def cglobalname(self):
    return self.name.replace('.', '_')

  def __hash__(self):
    if not hasattr(self, 'name'):
      return hash(None)
    else:
      return hash(self.name)

  def __eq__(self, other):
    if not hasattr(self, 'name'):
      return False
    else:
      return self.name == other.name

  def __ne__(self, other):
    return not self.__eq__(other)

  def deref(self, access):
    raise errors.TypeError("Cannot dereference type '%s', at %s" % (self.name, self.codeloc))

  def typecheck(self):
    return self

  def firstpass(self):
    pass

  def unboundgeneric(self):
    return False

  def mapgeninsts(self, aux):
    pass

  def geninst_action(self):
    return False, False

def _asexpr(name):
  path = name.split('.')
  path[0] = ast.ExprValue(path[0])
  return reduce(lambda a, b: ast.ExprField(a, '.', ast.ExprValue(b)), path)

class Type(Typename):
  def __init__(self, defn):
    super(Type, self).__init__(str(defn.scope))
    assert defn is not None
    self.defn = defn

  def concrete_definition(self):
    return self.defn

  def isa(self, typeconstraint):
    assert not isinstance(typeconstraint, TypeUnboundGeneric)

    if type(self) != type(typeconstraint):
      return False
    if self == typeconstraint:
      return True

    if _isliteral(typeconstraint):
      try:
        _unify_lit_conc(typeconstraint, self)
        return True
      except:
        pass

    with scope.push(self.defn.scope):
      for i in self.defn.listisa:
        if i.typecheck().isa(typeconstraint):
          return True
    return False

  def asexpr(self):
    return _asexpr(self.name)

class TypeUnboundGeneric(Typename):
  def __init__(self, defn, *args):
    super(TypeUnboundGeneric, self).__init__(str(defn.scope))
    self.defn = defn
    self.args = list(args)

  def concrete_definition(self):
    return self.defn

class TypeRef(Typename):
  def __init__(self, access, type, nullable=False):
    prefix = '@'
    if access == '!':
      prefix = '@!'
    if nullable:
      prefix = '?' + prefix
    super(TypeRef, self).__init__(prefix + type.name)
    self.access = access
    self.type = type
    self.nullable = nullable

  def deref(self, access):
    if self.access != '!' and access == '!':
      raise errors.TypeError("Cannot mutate the type '%s', at %s" % (self, self.codeloc))
    return self.type

  def concrete_definition(self):
    return self.deref(self.access).concrete_definition()

  def isa(self, typeconstraint):
    assert not isinstance(typeconstraint, TypeUnboundGeneric)
    if type(self) != type(typeconstraint):
      return False
    if self == typeconstraint:
      return True
    if typeconstraint.access == '!' and self.access == '.':
      return False
    if not typeconstraint.nullable and self.nullable:
      return False
    return self.type.isa(typeconstraint.type)

class TypeTuple(Typename):
  def __init__(self, *args):
    super(TypeTuple, self).__init__('(' + ', '.join([str(t) for t in args]) + ')')
    self.args = args
    self.geninst = ast.TupleInstance(self)

  def isa(self, typeconstraint):
    assert not isinstance(typeconstraint, TypeUnboundGeneric)
    if type(self) != type(typeconstraint):
      return False
    if self == typeconstraint:
      return True
    for u,v in zip(self.args, typeconstraint.args):
      if not u.isa(v):
        return False
    return True

  def geninst_action(self):
    return True, True

class TypeApp(Typename):
  def __init__(self, defn, *args):
    super(TypeApp, self).__init__('(' + str(defn.scope) + ' ' + ' '.join([str(t) for t in args]) + ')')
    self.defn = defn
    for t in args:
      assert isinstance(t, Typename)
    self.args = list(args)
    self.geninst = ast.GenericInstance(ast.ExprCall(_asexpr(str(self.defn.scope)), self.args))
    self._fillscope()

  def _fillscope(self):
    self.genscope = scope.Scope(self)
    for genarg, t in zip(self.defn.instantiable_genargs(), self.args):
      self.genscope.define(t, name=genarg.name)

  def concrete_definition(self):
    return self.defn

  def isa(self, typeconstraint):
    assert isinstance(typeconstraint, Typename) and not isinstance(typeconstraint, TypeUnboundGeneric)
    if type(self) != type(typeconstraint):
      return False
    if self == typeconstraint:
      return True

    if str(self.defn.scope) != str(typeconstraint.defn.scope):
      return False

    found = False
    with scope.push(self.defn.scope):
      for i in self.defn.listisa:
        if i.typecheck().isa(typeconstraint):
          found = True
          break

    if not found:
      return False

    for u,v in zip(self.args, typeconstraint.args):
      if not u.isa(v):
        return False
    return True

  def geninst_action(self):
    return True, True

class TypeFunction(Typename):
  def __init__(self, defn, rettype, *args):
    pd = defn.scope.parent_definition.container
    if pd is not None and isinstance(pd, ast.TypeDecl):
      name = str(pd.typecheck()) + '.' + defn.name
    else:
      name = str(defn.scope)

    if isinstance(defn, ast.MethodDecl):
      kind = 'method' + defn.access
    else:
      kind = 'fun'

    super(TypeFunction, self).__init__('(' + kind + ' ' + name + ' ' + ' '.join([str(t) for t in args]) + ' = ' + str(rettype) + ')')
    self.defn = defn
    self.rettype = rettype
    self.args = list(args)
    self._fillscope()

  def _fillscope(self):
    self.genscope = scope.Scope(self)
    for genarg, t in zip(self.defn.instantiable_genargs(), self.args):
      self.genscope.define(t, name=genarg.name)

  def concrete_definition(self):
    return self.defn


def _unifyerror(*types):
  raise errors.TypeError("Cannot unify the types %s\nFrom locations:\n%s\n" \
      % (map(str, types), '\n'.join([str(t.codeloc) for t in types])))

def _unifyliterals(lits):
  if len(lits) == 0:
    return None

  if len(set(lits)) > 1:
    _unifyerror(*lits)
  return lits[0]

def _unify_lit_conc(lit, conc):
  nummod = '<root>.nlang.numbers.'
  nump = conc.name.startswith(nummod)
  if lit.name == '<root>.nlang.literal.integer':
    if nump and conc.name[len(nummod):] in \
        ['u8', 'u16', 'u32', 'u64',
            'i8', 'i16', 'i32', 'i64',
            'size', 'ssize']:
      return conc
  elif lit.name == '<root>.nlang.literal.bool':
    if nump and conc.name[len(nummod):] == 'bool':
      return conc
  elif lit.name == '<root>.nlang.literal.string':
    if conc.name == '<root>.nlang.string.string' or conc.name == '<root>.nlang.char.char':
      return conc
  elif lit.name == '<root>.nlang.literal.nulltype':
    if conc.nullable:
      return conc

  _unifyerror(lit, conc)

def _isliteral(type):
  return type.name.startswith('<root>.nlang.literal.')

def _isconcrete(type):
  return not type.name.startswith('<root>.nlang.literal.') and type.name != '<root>.nlang.meta.alias'

def _handlethis(type):
  if type is None:
    return None
  elif type.name == 'this':
    return scope.current().q(type)
  else:
    return type

def _unifyconcpair(a, b):
  if a.isa(b):
    return b
  elif b.isa(a):
    return a
  else:
    return None

def unify(constraint, types):
  assert len(types) > 0
  types = map(_handlethis, types)
  constraint = _handlethis(constraint)

  ulit = _unifyliterals(filter(_isliteral, types))

  concretes = set(filter(_isconcrete, types))
  if len(concretes) > 1:
    common = None
    for c in concretes:
      for i in concretes:
        u = _unifyconcpair(c, i)
        if u is None:
          common = None
          break
        if common is None:
          common = u
        else:
          common = _unifyconcpair(common, u)
      if common is None:
        break
    if common is None:
      _unifyerror(*concretes)
    else:
      concretes = set([common])

  if len(concretes) > 1:
    _unifyerror(concretes)

  if len(concretes) == 0:
    c = None
  else:
    c = concretes.pop()

  t = None
  if ulit is not None and c is not None:
    t = _unify_lit_conc(ulit, c)
  elif ulit is not None and c is None:
    t = ulit
  elif c is not None:
    t = c

  if t is None:
    raise errors.TypeError()
  elif constraint is None:
    return t
  elif constraint == qbuiltin('nlang.meta.alias'):
    return t
  elif _isliteral(t):
    return _unify_lit_conc(t, constraint)
  elif not t.isa(constraint):
    raise errors.TypeError()
  else:
    return t

def checkcompat(constraint, type):
  return unify(constraint, [type])
