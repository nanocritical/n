import re
import errors
import scope
import ast
import copy

def qbuiltin(name):
  x = ast.path_as_expr(name)
  return scope.current().q(x).typecheck()

class Typename(object):
  def __init__(self, name):
    self.name = Typename.normalize(name)
    self.codeloc = ast.CodeLoc(self)
    self.literal_value = None

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

  def mapgeninsts(self, aux, memo):
    pass

  def geninst_action(self):
    return False, False

  def ref_type(self):
    return None

  def is_some_ref(self):
    return False

  def has_instantiable(self):
    return False

  def definition(self):
    return self.concrete_definition()

  def __deepcopy__(self, memo):
    memo[id(self)] = self
    return self

  def gather_temporaries(self, tmps):
    pass

  def has_storage(self):
    return False

def _asexpr(name):
  path = name.split('.')
  path[0] = ast.ExprValue(path[0])
  return reduce(lambda a, b: ast.ExprField(a, '.', ast.ExprValue(b)), path)

class Type(Typename):
  def __init__(self, defn):
    super(Type, self).__init__(str(defn.scope))
    assert self.name != 'size'
    assert defn is not None
    self.defn = defn

  def set_literal_value(self, lit):
    if lit is not None:
      self.literal_value = lit

  def concrete_definition(self):
    return self.defn

  def isa(self, typeconstraint):
    assert not isinstance(typeconstraint, TypeUnboundGeneric)

    if type(self) != type(typeconstraint):
      return False
    if self == typeconstraint:
      return True

    if _isliteral(self):
      try:
        _unify_lit_conc(self, typeconstraint)
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

  def has_storage(self):
    return self.defn.has_storage()

class TypeUnboundGeneric(Typename):
  def __init__(self, defn, *args):
    super(TypeUnboundGeneric, self).__init__(str(defn.scope))
    self.defn = defn
    self.args = list(args)

  def concrete_definition(self):
    raise Exception("Unbound generic '%s' does not have a concrete definition, at %s" \
        % (self, self.codeloc))

  def has_storage(self):
    return False

class TypeTuple(Typename):
  def __init__(self, *args):
    super(TypeTuple, self).__init__('(' + ', '.join([str(t) for t in args]) + ')')
    self.args = args
    self.geninst = ast.TupleInstance(self)
    self.defn = self.geninst

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

  def itersubnodes(self, **kw):
    return ast._itersubnodes(self.args + [self.geninst], **kw)

  def has_storage(self):
    return True

class Refs(object):
  ANY_REF, REF, MUTABLE_REF, NULLABLE_REF, NULLABLE_MUTABLE_REF = range(5)
  PREFIXES = { REF: '@', MUTABLE_REF: '@!', NULLABLE_REF: '?@', NULLABLE_MUTABLE_REF: '?@!' }
  CAN_DEREF = { REF: '.', MUTABLE_REF: '!' }
  BY_FULLNAME = {
    '<root>.nlang.meta.any_ref': ANY_REF,
    '<root>.nlang.meta.ref': REF,
    '<root>.nlang.meta.mutable_ref': MUTABLE_REF,
    '<root>.nlang.meta.nullable_ref': NULLABLE_REF,
    '<root>.nlang.meta.nullable_mutable_ref': NULLABLE_MUTABLE_REF,
  }

class TypeApp(Typename):
  def __init__(self, defn, *args):
    names = []
    for t in args:
      if t.literal_value is not None:
        names.append(str(t.literal_value))
      else:
        names.append(str(t))
    super(TypeApp, self).__init__('(' + str(defn.scope) + ' ' + ' '.join(names) + ')')
    assert defn is not None
    self.defn = defn
    for t in args:
      assert isinstance(t, Typename)
    self.args = list(args)
    self.geninst = defn.geninst

    self._ref = Refs.BY_FULLNAME.get(str(self.defn.scope), None)
    prefix = Refs.PREFIXES.get(Refs.BY_FULLNAME.get(str(self.defn.scope)), None)
    if prefix is not None:
      Typename.__init__(self, prefix + str(self.args[0]))

  def concrete_definition(self):
    return self.defn

  def isa(self, typeconstraint):
    assert isinstance(typeconstraint, Typename) and not isinstance(typeconstraint, TypeUnboundGeneric)
    if self == typeconstraint:
      return True

    found = str(self.defn.scope) == str(typeconstraint.defn.scope)

    if not found:
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

  def itersubnodes(self, **kw):
    return ast._itersubnodes([self.defn] + self.args, **kw)

  def is_some_ref(self):
    return self._ref is not None

  def ref_type(self):
    return self._ref

  def deref(self, access):
    if not self.is_some_ref():
      raise errors.TypeError("Cannot dereference '%s', at %s" % (self, self.codeloc))

    allowed = Refs.CAN_DEREF.get(Refs.BY_FULLNAME.get(str(self.defn.scope)), None)
    if allowed is None:
      raise errors.TypeError("Cannot dereference '%s', at %s" % (self, self.codeloc))
    elif allowed == '.' and access == '!':
      raise errors.TypeError("Cannot mutate '%s', at %s" % (self, self.codeloc))
    else:
      return self.args[0]

  def has_storage(self):
    return True

class SomeRefInstance(object):
  def __init__(self, defn):
    super(SomeRefInstance, self).__init__()
    self.defn = defn

def _instantiate_ref(d, type):
  genenv = ast.GenericEnv()
  genarg = d.type.args[0]
  with scope.push(d.scope):
    genarg.typedestruct(genenv, type)
  defn, new_instance = d.mk_instantiated_copy(genenv)
  defn.geninst = SomeRefInstance(defn)
  if new_instance:
    defn.firstpass()
    ast.ctx().gen_instances_fwd.add(defn.typecheck())
  return defn

g_ref_type_cache = {}

def mk_some_ref(name, type):
  global g_ref_type_cache
  cookie = name + ' ' + str(type)
  cached = g_ref_type_cache.get(cookie, None)
  if cached is not None:
    cpy = copy.copy(cached)
    cpy.codeloc = type
    return cpy

  defn = _instantiate_ref(ast.g_builtin_defs[name], type)
  t = TypeApp(defn, type)
  g_ref_type_cache[cookie] = t
  return t

def mk_any_ref(type):
  return mk_some_ref('<root>.nlang.meta.any_ref', type)

def mk_ref(type):
  return mk_some_ref('<root>.nlang.meta.ref', type)

def mk_mutable_ref(type):
  return mk_some_ref('<root>.nlang.meta.mutable_ref', type)

def mk_nullable_ref(type):
  return mk_some_ref('<root>.nlang.meta.nullable_ref', type)

def mk_nullable_mutable_ref(type):
  return mk_some_ref('<root>.nlang.meta.nullable_mutable_ref', type)

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

  def concrete_definition(self):
    return self.defn


def _unifyerror(types):
  raise errors.TypeError("Cannot unify the types %s\nFrom locations:\n%s\n" \
      % (map(str, types), '\n'.join([str(t.codeloc) for t in types])))

def _unifyliterals(lits):
  if len(lits) == 0:
    return None

  if len(set(lits)) > 1:
    _unifyerror(lits)
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
    if conc.name == '<root>.nlang.stringmod.string' or conc.name == '<root>.nlang.charmod.char':
      return conc
  elif lit.name == '<root>.nlang.literal.nulltype':
    if conc.name.startswith('?'):
      return conc

  _unifyerror([lit, conc])

def _isliteral(type):
  return type.name.startswith('<root>.nlang.literal.')

def _isconcrete(type):
  return not type.name.startswith('<root>.nlang.literal.') and type.name != '<root>.nlang.meta.alias'

def _istuple(type):
  return isinstance(type, TypeTuple)

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

def _unify_tuples(constraint, types):
  if constraint is not None:
    if not _istuple(constraint):
      _unifyerror([constraint] + types)
    size = len(constraint.args)
  else:
    size = len(types[0].args)

  for t in types:
    if len(t.args) != size:
      _unifyerror(list)

  if constraint is None:
    c = [None for _ in range(size)]
  else:
    c = constraint.args

  r = []
  for i in range(size):
    each = []
    for t in types:
      each.append(t.args[i])
    r.append(unify(c[i], each))

  return TypeTuple(*r)

def unify(constraint, types):
  assert len(types) > 0
  assert constraint is not None or len(types) > 1

  tuples = filter(_istuple, types)
  if len(tuples) > 0:
    if len(tuples) != len(types):
      _unifyerror([constraint] + types)
    return _unify_tuples(constraint, types)

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
      _unifyerror(concretes)
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
  elif c is None and constraint == qbuiltin('nlang.meta.alias'):
    return constraint
  else:
    raise Exception('Unexpected')

  if constraint is None:
    return t
  elif constraint == qbuiltin('nlang.meta.alias'):
    return t
  elif constraint == qbuiltin('nlang.literal.integer') and _isliteral(t):
    if constraint.literal_value != t.literal_value:
      _unifyerror([constraint, t])
  elif _isliteral(t):
    return _unify_lit_conc(t, constraint)

  if ((t.ref_type() == Refs.REF and constraint.ref_type() == Refs.NULLABLE_REF) \
      or (t.ref_type() == Refs.MUTABLE_REF and constraint.ref_type() == Refs.NULLABLE_MUTABLE_REF)) \
      and t.args[0].isa(constraint.args[0]):
    return t
  elif not t.isa(constraint):
    _unifyerror([constraint, t])

  return t

def checkcompat(constraint, type):
  return unify(constraint, [type])
