import re
import errors
import scope
import ast

def qbuiltin(name):
  path = name.split('.')
  path[0] = ast.ExprField(ast.ExprValue('<root>'), '.', ast.ExprValue(path[0]))
  x = reduce(lambda a, b: ast.ExprField(a, '.', ast.ExprValue(b)), path)
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
    return hash(self.name)

  def __eq__(self, other):
    return self.name == other.name

  def __ne__(self, other):
    return self.name != other.name

  def deref(self, access):
    raise errors.TypeError("Cannot dereference type '%s', at %s" % (self.name, self.codeloc))

  def typecheck(self):
    return self

  def firstpass(self):
    pass

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

    if self == typeconstraint:
      return True

    if _isliteral(typeconstraint):
      try:
        _unify_lit_conc(typeconstraint, self)
        return True
      except:
        pass

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
    super(TypeRef, self).__init__(access + type.name)
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
    if not self.defn.isa(typeconstraint):
      if str(self.defn.scope) == str(typeconstraint.defn.scope):
        return True
      for i in self.defn.listisa:
        if i.isa(typeconstraint):
          return True
      return False
    for u,v in zip(self.args, typeconstraint.args):
      if not u.isa(v):
        return False

  def geninst_action(self):
    return True, True

class TypeFunction(Typename):
  def __init__(self, defn, rettype, *args):
    super(TypeFunction, self).__init__('(fun' + str(defn.scope) + ' ' + ' '.join([str(t) for t in args]) + ' = ' + str(rettype) + ')')
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
  if lit.name == '<root>.nlang.literal.Integer':
    if nump and conc.name[len(nummod):] in \
        ['U8', 'U16', 'U32', 'U64',
            'I8', 'I16', 'I32', 'I64',
            'Size', 'SSize']:
      return conc
  elif lit.name == '<root>.nlang.literal.Bool':
    if nump and conc.name[len(nummod):] == 'Bool':
      return conc
  elif lit.name == '<root>.nlang.literal.String':
    if conc.name == '<root>.nlang.string.String' or conc.name == '<root>.nlang.numbers.Char':
      return conc
  elif lit.name == '<root>.nlang.literal.Null':
    if conc.nullable:
      return conc

  _unifyerror(lit, conc)

def _isliteral(type):
  return type.name.startswith('<root>.nlang.literal.')

def _isconcrete(type):
  return not type.name.startswith('<root>.nlang.literal.') and type.name != '<root>.nlang.meta.Type'

def _handlethis(type):
  if type.name == 'this':
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

def unify(types):
  assert len(types) > 0
  types = map(_handlethis, types)

  ulit = _unifyliterals(filter(_isliteral, types))

  concretes = filter(_isconcrete, types)
  if len(set(concretes)) > 1:
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
      concretes = [common]

  if ulit is not None and len(concretes) > 0:
    return _unify_lit_conc(ulit, concretes[0])
  elif ulit is not None:
    return ulit
  elif len(concretes) > 0:
    return concretes[0]

def checkcompat(target, type):
  target = _handlethis(target)
  return unify([target, type])
