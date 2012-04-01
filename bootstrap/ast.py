import re
import copy
import errors
import scope
import parser
import typing

gnextsym = 0
g_static_init = []

def gensym():
  global gnextsym
  n = gnextsym
  gnextsym += 1
  return '__nlang_gensym' + str(n) + '__'

def path_as_expr(name):
  path = name.split('.')
  path[0] = ExprField(ExprValue('<root>'), '.', ExprValue(path[0]))
  return reduce(lambda a, b: ExprField(a, '.', ExprValue(b)), path)

def deepset_codeloc(node, codeloc):
  node.codeloc = codeloc
  for n in node.itersubnodes(onelevel=True):
    n.codeloc = codeloc

class _QueryWrapper(object):
  def __init__(self, name):
    self.name = name
    self.codeloc = CodeLoc(self)

_gshallowfence = set()

class _ShallowFence(object):
  def __init__(self, x):
    global _gshallowfence
    self.idx = id(x)
    if self.idx in _gshallowfence:
      self.fenced = True
    else:
      self.fenced = False
      _gshallowfence.add(self.idx)

  def __enter__(self):
    return self.fenced

  def __exit__(self, *args):
    global _gshallowfence
    if not self.fenced:
      _gshallowfence.remove(self.idx)

def _prettystr(x):
  with _ShallowFence(x) as fenced:
    if fenced:
      return repr(x)

    if isinstance(x, dict):
      s = '{ '
      for k, v in x.iteritems():
        s += "'%s': %s, " % (k, v)
      return s + '}'
    elif isinstance(x, tuple):
      s = '('
      for v in x:
        s += _prettystr(v) + ', '
      return s + ')'
    elif isinstance(x, list):
      s = '['
      for v in x:
        s += _prettystr(v) + ', '
      return s + ']'
    elif isinstance(x, basestring):
      return '"' + x + '"'
    elif isinstance(x, scope.Scope):
      return str(x)
    elif isinstance(x, CodeLoc):
      return str(x)
    elif hasattr(x, '__dict__') and hasattr(x, 'name') and hasattr(x, 'codeloc'):
      return x.__class__.__name__ + '(' + str(x.codeloc) + ')'
    elif hasattr(x, '__dict__'):
      s = x.__class__.__name__ + '{ '
      for k, v in x.__dict__.iteritems():
        s += "'%s': %s, " % (k, _prettystr(v))
      return s + '}'
    else:
      return str(x)

def _listwrap(x):
  if isinstance(x, list):
    return x
  else:
    return [x]

def _itersubnodes(*args, **kw):
  filter_out = kw.pop('filter_out', (lambda n: False))
  onelevel = kw.get('onelevel', False)
  for a in args:
    if a is None:
      continue
    for g in a:
      if g is None:
        continue
      if filter_out(g):
        continue
      yield g
      if onelevel:
        continue
      for sub in g.itersubnodes(**kw):
        if filter_out(sub):
          continue
        yield sub

class CodeLoc(object):
  def __init__(self, obj):
    self.obj = obj
    global gmodname
    global gmodctx
    self.fn = gmodctx[gmodname[-1]].fn
    self.line = gmodctx[gmodname[-1]].line

  def __str__(self):
    s = self.fn + ':' + str(self.line)
    if hasattr(self.obj, 'name'):
      s += ': ' + self.obj.name
    return s

class _Node(object):
  def __init__(self):
    self.cachedtype = None

  def itersubnodes(self, **kw):
    raise Exception("Not implemented in '%s'" % type(self))

  def mapgeninsts(self, aux):
    for n in self.itersubnodes(onelevel=True):
      isgeninst, descend = n.geninst_action()
      sc = None
      if hasattr(n, 'scope') and not isinstance(n, VarDecl):
        sc = n.scope

      with scope.push(sc):
        if isgeninst:
          aux(n.geninst)
        if descend and not n.unboundgeneric():
          n.mapgeninsts(aux)

  def geninst_action(self):
    return False, True

  def firstpass(self):
    if hasattr(self, 'scope'):
      sc = self.scope
    else:
      sc = None

    with scope.push(sc):
      for n in self.itersubnodes(onelevel=True):
        n.firstpass()
    self._validate()

  def _validate(self):
    pass

  def typecheck(self, *args, **kw):
    if self.cachedtype is None:
      type = self.nocache_typecheck(*args, **kw)
      assert isinstance(type, typing.Typename)
      if not isinstance(type, typing.TypeUnboundGeneric):
        self.cachedtype = type
      type.codeloc = self.codeloc
      return type
    else:
      return self.cachedtype

  def definition(self):
    return scope.current().q(self).definition()

  def concrete_definition(self):
    return self.typecheck().concrete_definition()

  def unboundgeneric(self):
    return False

def _is_non_function_primitive(node):
  return isinstance(node, ExprCall) \
      and node.geninst.defn is not None \
      and (str(node.geninst.defn.scope) == '<root>.nlang.prelude._sizeof' \
           or str(node.geninst.defn.scope) == '<root>.nlang.unsafe.cast')

class _IsGenericInstance(object):
  def geninst_action(self):
    return not _is_non_function_primitive(self), True

class _NameEq(_Node):
  def __init__(self, name, scope=None):
    _Node.__init__(self)
    self.name = name
    self.codeloc = CodeLoc(self)

  def __str__(self):
    return self.name

  def __hash__(self):
    return hash(self.name)

  def __eq__(self, other):
    return self.name == other.name

  def __ne__(self, other):
    return self.name != other.name

class _FieldsEq(_Node):
  def __init__(self):
    _Node.__init__(self)
    self.codeloc = CodeLoc(self)

  def __str__(self):
    return _prettystr(self)

  def __hash__(self):
    return hash(str(self))

  def __eq__(self, other):
    return str(self) == str(other)

  def __ne__(self, other):
    return str(self) != str(other)

class CGlobalName(object):
  pass

class GenericArg(_NameEq):
  def __init__(self, name, typeconstraint=None):
    super(GenericArg, self).__init__(name)
    self.typeconstraint = typeconstraint

  def instantiated(self):
    try:
      r = scope.current().q(self)
      if r is None:
        return None
      else:
        assert False
        return r.typecheck()
    except errors.ScopeError, ignore:
      return None

  def setinstantiated(self, genscope, t):
    if self.name in genscope.table:
      typing.checkcompat(genscope.table[self.name].typecheck(), t)
    else:
      genscope.define(GenericArgInstantiated(self, t))

  def typedestruct(self, genscope, t):
    if self.typeconstraint is None:
      self.setinstantiated(genscope, t)
    else:
      assert not isinstance(t, typing.TypeRef)
      self.typeconstraint.typedestruct(genscope, t)
      self.setinstantiated(genscope, t)

  def itersubnodes(self, **kw):
    return _itersubnodes([self.typeconstraint], **kw)

  def nocache_typecheck(self):
    raise 'Invalid'

  def definition(self):
    return None

  def __deepcopy__(self, memo):
    global gdisable_instantiatedcopy
    global ginstantiatedcopy_sub
    if not gdisable_instantiatedcopy and self.name in ginstantiatedcopy_sub:
      cpy = ginstantiatedcopy_sub[self.name]
    else:
      cpy = GenericArg(self.name, copy.deepcopy(self.typeconstraint))
    cpy.codeloc = self.codeloc
    cpy.codeloc.obj = cpy
    return cpy

class GenericTypename(_NameEq):
  def __init__(self, type, *args):
    super(GenericTypename, self).__init__(type.name)
    self.type = type
    self.args = list(args)
    self.defn = None

  def typedestruct(self, genscope, t):
    self.type.typedestruct(genscope, t)
    for a, b in zip(self.args, t.args):
      a.typedestruct(genscope, b)

  def nocache_typecheck(self):
    return typing.TypeApp(self.defn, *[a.typecheck() for a in self.args])

  def itersubnodes(self, **kw):
    return _itersubnodes(self.args, **kw)

  def is_meta_type(self):
    return True

class Decl(object):
  pass

gdisable_instantiatedcopy = False
ginstantiatedcopy_sub = {}

class TypeDef(_NameEq):
  def __init__(self, name):
    super(TypeDef, self).__init__(name)
    if not isinstance(self, FunctionDecl) and not isinstance(self, ChoiceDecl):
      self.type.defn = self
    if not hasattr(self, 'genargs'):
      self.genargs = []
    if not hasattr(self, 'type'):
      self.type = None
    self.unbound = len(self.genargs) > 0 or isinstance(self.type, GenericTypename)

  def unboundgeneric(self):
    return self.unbound

  def firstpass(self):
    if self.unboundgeneric():
      return
    with scope.push(self.scope):
      for n in self.itersubnodes(onelevel=True):
        n.firstpass()
    self._validate()

  def definition(self):
    return self

  def nocache_typecheck(self, **ignored):
    if self.unboundgeneric():
      return typing.TypeUnboundGeneric(self, *self.type.args)
    else:
      if isinstance(self.type, GenericTypename):
        return self.type.nocache_typecheck()
      else:
        return typing.Type(self)

  def instantiable_genargs(self):
    if isinstance(self, FunctionDecl):
      return self.genargs
    elif isinstance(self.type, GenericTypename):
      return self.genargs + self.type.args
    else:
      return []

  def instantiated_copy(self, genscope):
    memo = {}
    memo[id(self.scope.parent)] = self.scope.parent
    memo[id(self.scope.parent_definition)] = self.scope.parent_definition

    global ginstantiatedcopy_sub
    ginstantiatedcopy_sub = {}
    for a in self.instantiable_genargs():
      ginstantiatedcopy_sub[a.name] = genscope.table[a.name]
    cpy = copy.deepcopy(self, memo)
    ginstantiatedcopy_sub = {}
    cpy.unbound = False
    return cpy

class Intf(TypeDef, Decl):
  def __init__(self, type, genargs, listisa, typedecls, decls, methods, funs):
    self.type = type
    self.genargs = genargs
    super(Intf, self).__init__(type.name)
    self.listisa = listisa
    self.typedecls = typedecls
    self.decls = decls
    self.methods = methods
    self.funs = funs
    self._fillscope()

  def _fillscope(self):
    self.scope = scope.Scope(self)
    if isinstance(self.type, GenericTypename):
      for a in self.type.args:
        self.scope.define(a)
    self.scope.define(self.type, name='this')
    for a in self.typedecls:
      self.scope.define(a)
    for a in self.decls:
      self.scope.define(a)
    for a in self.methods:
      self.scope.define(a)
    for a in self.funs:
      self.scope.define(a)

  def exportables(self):
    return self.typedecls + self.decls + self.funs + self.methods

  def itersubnodes(self, **kw):
    return _itersubnodes(self.listisa, self.typedecls, self.decls, self.methods, self.funs, **kw)

class ChoiceDecl(TypeDef, CGlobalName):
  def __init__(self, choice, typearg=None, value=None):
    super(ChoiceDecl, self).__init__(choice)
    self.value = value
    self.typearg = typearg
    self.defn = None

  def _defbuiltins(self):
    if self.typearg is not None:
      args = [VarDecl(ExprConstrained(ExprValue('arg'), self.typearg))]
    else:
      args = []
    init = ExprInitializer(self.defn.type,
        [('_which', ExprLiteral(self.value))])
    self.mk = FunctionDecl('mk', [], args, [self.defn.type], ExprBlock([ExprReturn(init)]))
    self.valuevar = FieldStaticConstDecl( \
        PatternDecl(ExprConstrained(ExprValue('value'), ExprValue('u32')), \
            ExprLiteral(self.value)))
    self._fillscope()

  def _fillscope(self):
    self.scope = scope.Scope(self)
    self.scope.define(self.mk)
    self.scope.define(self.valuevar)

  def nocache_typecheck(self, **ignored):
    return self.defn.typecheck()

  def itersubnodes(self, **kw):
    return _itersubnodes([self.typearg], **kw)

class Inherit(_FieldsEq):
  def __init__(self, type, interfaces=None):
    self.type = type
    self.interfaces = interfaces

  def itersubnodes(self, **kw):
    return _itersubnodes([self.type, self.interfaces], **kw)

def _duplicate_funargs(args):
  r = []
  for a in args:
    b = VarDecl(ExprConstrained(a, a.type))
    b.optionalarg = a.optionalarg
    r.append(b)
  return r

def _filterout_static_decls(userdecls):
  f = lambda d: isinstance(d, PatternDecl)
  decls, statics = filter(lambda d: not f(d), userdecls), filter(f, userdecls)
  return decls, map(FieldStaticConstDecl, statics)

class TypeDecl(TypeDef, Decl, CGlobalName):
  REC, TAGGEDUNION, ENUM, UNION, FORWARD = range(5)

  def __init__(self, type, genargs, listisa, inherits, typedecls, userdecls, methods, funs):
    self.type = type
    self.genargs = genargs  # Free type vars not in self.type.args
    super(TypeDecl, self).__init__(type.name)
    self.listisa = listisa
    self.inherits = inherits
    self.typedecls = typedecls

    for m in methods:
      m.container = self

    self.userdecls, self.static_decls = _filterout_static_decls(userdecls)
    self.decls = self.userdecls[:]
    self.declnum = None

    self.methods = methods
    self.funs = funs

    self.scope = scope.Scope(self)
    self._fillscope_basic()

  def _fillscope_basic(self):
    self.all_genargs = []
    if isinstance(self.type, GenericTypename):
      for a in self.genargs:
        self.scope.define(a)
        self.all_genargs.append(a)
      for a in self.type.args:
        self.scope.define(a)
        self.all_genargs.append(a)
    self.scope.define(self.type, name='this')

  def _fillscope_fun(self, f):
    self.scope.define(f)
    f.scope.define(self.type, name='this')
    for ga in self.all_genargs:
      f.scope.define(ga, noparent=True)

  def _fillscope_method(self, m):
    self._fillscope_fun(m)
    m.scope.define(VarDecl(ExprConstrained( \
        ExprValue('self'), ExprRef(m.access, self.type))))

  def _fillscope_members(self):
    for a in self.typedecls:
      self.scope.define(a)
    for a in self.decls:
      self.scope.define(a)
    for a in self.static_decls:
      self.scope.define(a)
    self.scope.define(self.declnum)
    for a in self.methods:
      self._fillscope_method(a)
    for a in self.funs:
      self._fillscope_fun(a)

  def firstpass(self):
    if self.unboundgeneric():
      return

    with scope.push(self.scope):
      for n in self.inherits:
        n.firstpass()

    self._do_inherit()
    self._generate_choice_builtins()
    self._fillscope_members()
    self._fixscope()

    with scope.push(self.scope):
      for n in self.itersubnodes(onelevel=True, filter_out=(lambda n: not isinstance(n, FieldDecl))):
        n.firstpass()

      if not self.unboundgeneric() and self.kind != TypeDecl.FORWARD:
        self._generate_builtins()

      for n in self.itersubnodes(onelevel=True, filter_out=(lambda n: isinstance(n, FieldDecl))):
        n.firstpass()

    ctx().gen_instances_fwd.add(self.typecheck())
    self._validate()

  def _copy_inherited(self, inh):
    inhd = inh.type.definition()

    memo = {}
    memo[id(inhd)] = self
    memo[id(inhd.scope.parent)] = self.scope.parent
    memo[id(inhd.scope.parent_definition)] = self.scope.parent_definition

    protected = set('mk new _ctor _dtor'.split())
    def aux(memo, dst, src):
      for d in src:
        if d.name.startswith('__') or d.name in protected:
          continue

        global gdisable_instantiatedcopy
        gdisable_instantiatedcopy = True
        cpy = copy.deepcopy(d, memo)
        gdisable_instantiatedcopy = False

        if hasattr(cpy, 'scope'):
          cpy.scope.parent = None
          cpy.scope.parent_definition = None
          cpy.scope.table.pop('this', None)
          cpy.scope.table.pop('self', None)
        dst.append(cpy)

    aux(memo, self.userdecls, inhd.userdecls)
    aux(memo, self.decls, inhd.decls)
    aux(memo, self.static_decls, inhd.static_decls)
    aux(memo, self.methods, inhd.methods)
    aux(memo, self.funs, inhd.funs)

  def _do_inherit(self):
    for i in self.inherits:
      i.firstpass()
      self._copy_inherited(i)
    self.kind = self.whatkind(self.userdecls, self.typedecls, self.methods + self.funs)

  def _generate_choice_builtins(self):
    i = 0
    for choice in self.userdecls:
      if isinstance(choice, ChoiceDecl):
        choice.defn = self
        if choice.value is None:
          choice.value = i
          i += 1
        choice._defbuiltins()
      elif isinstance(choice, FieldDecl):
        choice.typedecl = self
    if self.kind == TypeDecl.TAGGEDUNION:
      union = Union(ExprValue('__as'), [], [UnionField(d.name, d.typearg) for d in self.userdecls])
      self.typedecls.append(union)
      self.decls.append(FieldDecl(self, \
          ExprValue('__unsafe_as'), ExprField(ExprValue('this'), '.', ExprValue(union.name))))
    if self.kind == TypeDecl.TAGGEDUNION or self.kind == TypeDecl.ENUM:
      self.decls.append(FieldDecl(self, ExprValue('_which'), ExprValue('u32')))
      self.declnum = FieldStaticConstDecl( \
          PatternDecl(ExprConstrained(ExprValue('NUM__'), ExprValue('u32')),
            ExprLiteral(len(self.userdecls))))

  def _generate_builtins(self):
    m, f = [], []
    c = self._generate_ctor()
    if c is not None:
      m.append(c)
    c = self._generate_dtor()
    if c is not None:
      m.append(c)
    f.append(self._generate_alloc())
    f.append(self._generate_mk())
    f.append(self._generate_new())
    c = self._generate_static_init()
    if c is not None:
      f.append(c)
      g_static_init.append(self.typecheck())

    for c in f:
      deepset_codeloc(c, self.codeloc)
      self._fillscope_fun(c)

    for c in m:
      deepset_codeloc(c, self.codeloc)
      self._fillscope_method(c)

    for c in f + m:
      c.firstpass()

    self.methods.extend(m)
    self.funs.extend(f)

  def _generate_ctor(self):
    xself = ExprValue('self')
    body = []
    for d in self.decls:
      if isinstance(d, FieldDecl):
        td = d.typecheck()
        if isinstance(td, typing.TypeRef):
          continue
        if '__unsafe_ctor__' in td.defn.scope.table:
          body.append(ExprCall(ExprField(ExprField(xself, '.', ExprValue(d.name)),
            '!', ExprValue('__unsafe_ctor__')), []))

    args = []
    if '_ctor' in self.scope.table:
      body.append(ExprCall(ExprField(xself, '!', '_ctor'), []))
      args = _duplicate_funargs(self.scope.table['_ctor'].args)

    if len(body) == 0:
      return None
    else:
      block = ExprBlock(body)
      return MethodDecl('__unsafe_ctor__', [], '!', args, [typing.qbuiltin('nlang.numbers.void')], block)

  def _generate_dtor(self):
    xself = ExprValue('self')
    body = []
    rdecls = self.decls[:]
    rdecls.reverse()
    for d in rdecls:
      if isinstance(d, FieldDecl):
        td = d.typecheck()
        if isinstance(td, typing.TypeRef):
          continue
        if '__unsafe_dtor__' in td.defn.scope.table:
          body.append(ExprCall(ExprField(ExprField(xself, '.', ExprValue(d.name)),
            '!', ExprValue('__unsafe_dtor__')), []))

    if '_dtor' in self.scope.table:
      body.append(ExprCall(ExprField(xself, '!', '_dtor'), []))

    if len(body) == 0:
      return None
    block = ExprBlock(body)
    return MethodDecl('__unsafe_dtor__', [], '!', [], [typing.qbuiltin('nlang.numbers.void')], block)

  def _generate_alloc(self):
    expr = ExprReturn(ExprCall(
        ExprCall(path_as_expr('nlang.unsafe.cast'),
          [ExprRef('!', ExprValue('this')),
            ExprRef('!', path_as_expr('nlang.numbers.u8'))]),
          [ExprCall(path_as_expr('nlang.unsafe.malloc'),
            [ExprCall(ExprSizeof(), [ExprValue('this')])])]))
    return FunctionDecl('__unsafe_alloc__', [], [], [ExprRef('!', ExprValue('this'))], ExprBlock([expr]))

  def _generate_mk(self):
    tmp = gensym()
    if '_ctor' in self.scope.table:
      mk_args = _duplicate_funargs(self.scope.table['_ctor'].args)
      ctor_args = [ExprValue(a.name) for a in mk_args]
      ctor_call = [ExprCall(ExprField(ExprValue(tmp), '!', ExprValue('__unsafe_ctor__')),
          ctor_args)]
    elif '__unsafe_ctor__' in self.scope.table:
      mk_args = []
      ctor_call = [ExprField(ExprValue(tmp), '!', ExprValue('__unsafe_ctor__'))]
      ctor_call[0].maybeunarycall = True
    else:
      mk_args = []
      ctor_call = []

    v = VarDecl(ExprValue(tmp), ExprInitializer(ExprValue('this'), []))
    if len(ctor_call) > 0:
      v.setmutatingblock(ExprBlock(ctor_call))

    return FunctionDecl('mk', [], mk_args, [ExprValue('this')],
        ExprBlock([v] + [ExprReturn(ExprValue(tmp))]))

  def _generate_new(self):
    tmp = gensym()
    if '_ctor' in self.scope.table:
      new_args = _duplicate_funargs(self.scope.table['_ctor'].args)
      ctor_args = [ExprValue(a.name) for a in new_args]
      ctor_call = [ExprCall(ExprField(ExprValue(tmp), '!', ExprValue('__unsafe_ctor__')),
          ctor_args)]
    elif '__unsafe_ctor__' in self.scope.table:
      new_args = []
      ctor_call = [ExprField(ExprValue(tmp), '!', ExprValue('__unsafe_ctor__'))]
      ctor_call[0].maybeunarycall = True
    else:
      new_args = []
      ctor_call = []

    alloc_call = ExprField(ExprValue('this'), '.', ExprValue('__unsafe_alloc__'))
    alloc_call.maybeunarycall = True
    return FunctionDecl('new', [], new_args, [ExprRef('!', ExprValue('this'))],
        ExprBlock([VarDecl(ExprValue(tmp), alloc_call)] + ctor_call \
            + [ExprReturn(ExprValue(tmp))]))

  def _generate_static_init(self):
    xthis = ExprValue('this')
    body = []
    for d in self.static_decls:
      if d.vardecl.expr is not None:
        body.append(ExprInitStaticConstField(d.vardecl.name, d.vardecl.expr))
      if d.vardecl.mutatingblock is not None:
        body.append(d.vardecl.mutatingblock)

    if len(body) == 0:
      return None
    else:
      block = ExprBlock(body)
      return FunctionDecl('__static_init__', [], [], [typing.qbuiltin('nlang.numbers.void')], block)

  def _fixscope(self):
    # In the body of a method, current().q() is looking up at the module level.
    # I.e. class members are not in scope, they must be accessed via self, this
    # or an absolute path.
    for a in self.methods:
      a.scope.parent = self.scope.parent
    for a in self.funs:
      a.scope.parent = self.scope.parent

  def whatkind(self, decls, typedecls, methods_funs):
    if not isinstance(decls, list):
      decls = [decls]
    if len(decls) == 0:
      for td in typedecls:
        if td.kind != TypeDecl.FORWARD:
          return TypeDecl.REC

      for mf in methods_funs:
        if mf.body is not None:
          return TypeDecl.REC

      return TypeDecl.FORWARD

    kind = None
    for d in decls:
      if isinstance(d, ChoiceDecl):
        if d.typearg is None:
          k = TypeDecl.ENUM
        else:
          k = TypeDecl.TAGGEDUNION
      else:
        k = TypeDecl.REC

      if kind is None:
        kind = k
      elif kind != k:
        if kind == TypeDecl.ENUM and k == TypeDecl.TAGGEDUNION:
          kind = k
        elif kind == TypeDecl.TAGGEDUNION and k == TypeDecl.ENUM:
          pass
        else:
          raise errors.ParseError("Type declaration must have uniform kind, at %s" \
              % self.codeloc)

    return kind

  def choicedecl(self, choice):
    assert isinstance(choice, ExprValue)
    for c in self.decls:
      if isinstance(c, ChoiceDecl) and c.name == choice:
        return c
    raise errors.TypeError("Invalid selector '%s' for '%s', at %s" \
        % (choice, self.scope, choice.codeloc))

  def exportables(self):
    return self.imported + self.typedecls + self.decls + self.funs + self.methods

  def itersubnodes(self, **kw):
    return _itersubnodes(self.listisa, self.inherits, self.typedecls, self.static_decls, self.decls, self.funs, self.methods, **kw)

class UnionField(_NameEq, CGlobalName):
  def __init__(self, name, type):
    super(UnionField, self).__init__(name)
    self.type = type

  def itersubnodes(self, **kw):
    return _itersubnodes([self.type], **kw)

class Union(TypeDef, Decl, CGlobalName):
  def __init__(self, type, genargs, fields):
    self.type = type
    self.genargs = genargs
    super(Union, self).__init__(type.name)
    self.listisa = []
    self.fields = fields
    self._fillscope()

  def _fillscope(self):
    self.scope = scope.Scope(self)
    for f in self.fields:
      if f is not None:
        self.scope.define(f)

  def itersubnodes(self, **kw):
    return _itersubnodes(self.fields, **kw)

class FunctionDecl(TypeDef, Decl, CGlobalName):
  def __init__(self, name, genargs, args, returns, body):
    self.genargs = genargs
    super(FunctionDecl, self).__init__(name)
    self.listisa = []
    self.args = args
    self.returns = returns
    self.body = body
    if name == 'main':
      self.body.main = True
    if len(self.returns) > 1:
      self.rettype = ExprTuple(*self.returns)
    else:
      self.rettype = self.returns[0]
    self._fillscope()

  def _fillscope(self):
    self.scope = scope.Scope(self)
    for a in self.genargs:
      self.scope.define(a)
    for a in self.args:
      if isinstance(a, VarDecl):
        self.scope.define(a)
    for a in self.returns:
      if isinstance(a, VarDecl):
        self.scope.define(a)
    if self.body is not None:
      self.scope.define(self.body)

  def _validate(self):
    for a in self.args:
      if a.optionalarg:
        t = a.typecheck()
        if not isinstance(t, typing.TypeRef) or not t.nullable:
          raise errors.ParseError(
              "Optional argument '%s' to '%s' is not a nullable reference, but of type '%s', at %s"
              % (a, self, t, self.codeloc))

  def nocache_typecheck(self):
    if self.unboundgeneric():
      return typing.TypeUnboundGeneric(self)
    else:
      with scope.push(self.scope):
        return typing.TypeFunction(self, self.rettype.typecheck(), *[a.typecheck() for a in self.args])

  def itersubnodes(self, **kw):
    return _itersubnodes(self.args, self.returns, [self.rettype, self.body], **kw)

class MethodDecl(FunctionDecl):
  def __init__(self, name, genargs, access, args, returns, body):
    self.container = None
    self.access = access
    super(MethodDecl, self).__init__(name, genargs, args, returns, body)

class VarDecl(_FieldsEq, Decl):
  def __init__(self, name, expr=None):
    super(VarDecl, self).__init__()
    if isinstance(name, ExprValue):
      self.name = name.name
      self.type = None
    elif isinstance(name, ExprConstrained):
      self.name = name.args[0].name
      self.type = name.type
    else:
      raise errors.ParseError("Malformed VarDecl at %s" % self.codeloc)

    self.expr = expr
    self.mutatingblock = None
    self.optionalarg = False
    self.scope = scope.Scope(self)

  def is_meta_type(self):
    return self.type is not None and self.type.typecheck() == typing.qbuiltin('nlang.meta.alias')

  def nocache_typecheck(self, statement=False):
    t = self.rawtypecheck()
    if statement:
      return typing.qbuiltin('nlang.numbers.void')
    else:
      return t

  def rawtypecheck(self):
    if self.type is not None:
      if self.expr is not None:
        if self.type.typecheck() == typing.qbuiltin('nlang.meta.alias'):
          # To support the expression: 'let typealias:nlang.meta.alias = SomeType'
          return self.expr.typecheck()
        else:
          typing.checkcompat(self.type.typecheck(), self.expr.typecheck())
      return self.type.typecheck()
    else:
      return self.expr.typecheck()

  def definition(self):
    if self.type is not None:
      if isinstance(self.type, typing.Typename):
        return self.type.concrete_definition()
      else:
        return self.type.definition()
    else:
      return self.expr.definition()

  def setmutatingblock(self, b):
    self.mutatingblock = b
    if self.mutatingblock is not None:
      self.scope.define(self.mutatingblock)

  def itersubnodes(self, **kw):
    return _itersubnodes([self.type, self.expr, self.mutatingblock], **kw)

  def revtypedestruct(self, genscope, t):
    selftype = self.typecheck()
    t.revtypedestruct(genscope, selftype)

  def single(self):
    return isinstance(self.name, ExprValue)

class PatternDecl(_FieldsEq, Decl):
  def __init__(self, pattern, expr=None):
    super(PatternDecl, self).__init__()
    self.pattern = pattern
    self.expr = expr
    self.codetmp = ExprValue(gensym())
    self.vars = [VarDecl(ExprConstrained(self.codetmp, ExprValue('void')), self.expr)]
    patternvars = self.pattern.declvars(self.codetmp)
    if len(patternvars) == 1:
      # Remove the unnecessary intermediate temporary.
      self.vars[0].name = patternvars[0].name
      self.vars[0].type = patternvars[0].type
    else:
      self.vars.extend(patternvars)
    self.mutatingblock = None
    self.static = False
    assert not (self.static and self.mutatingblock is not None)
    self.scope = scope.Scope(self)
    # Do not define the self.vars in self.scope: they need to be defined
    # in self.scope.parent.

  def is_meta_type(self):
    return isinstance(self.pattern, ExprConstrained) \
        and self.pattern.type.typecheck() == typing.qbuiltin('nlang.meta.alias')

  def patterntypecheck(self):
    if self.expr is None:
      return self.pattern.patterntypedestruct(None)
    else:
      xtype = self.expr.typecheck()
      return self.pattern.patterntypedestruct(xtype)

  def nocache_typecheck(self):
    return typing.qbuiltin('nlang.numbers.void')

  def setmutatingblock(self, b):
    self.mutatingblock = b
    if self.mutatingblock is not None:
      self.scope.define(self.mutatingblock)

  def firstpass(self):
    for n in _itersubnodes([self.expr], onelevel=True):
      n.firstpass()
    self.vars[0].type = self.patterntypecheck()
    for n in _itersubnodes([self.pattern, self.mutatingblock] + self.vars, onelevel=True):
      n.firstpass()
    self._validate()

  def itersubnodes(self, **kw):
    # Do expr before vars.
    return _itersubnodes([self.expr, self.pattern, self.mutatingblock] + self.vars, **kw)

class FieldStaticConstDecl(_NameEq, CGlobalName, Decl):
  def __init__(self, patterndecl):
    super(FieldStaticConstDecl, self).__init__(patterndecl.vars[0].name)
    assert len(patterndecl.vars) == 1
    self.vardecl = patterndecl.vars[0]
    self.scope = scope.Scope(self)

  def itersubnodes(self, **kw):
    return iter('')

  def nocache_typecheck(self, **ignored):
    # Scope change because we may need 'this'.
    with scope.push(self.scope):
      return self.vardecl.typecheck()

  def definition(self):
    with scope.push(self.scope):
      return self.vardecl.definition()

  def is_meta_type(self):
    return self.vardecl.is_meta_type()

class FieldDecl(VarDecl):
  def __init__(self, typedecl, name, type):
    super(FieldDecl, self).__init__(ExprConstrained(name, type))
    self.typedecl = typedecl
    delattr(self, 'scope')

  def definition(self):
    with scope.push(self.typedecl.scope):
      return super(FieldDecl, self).definition()

  def nocache_typecheck(self):
    with scope.push(self.typedecl.scope):
      return super(FieldDecl, self).nocache_typecheck()

class GenericArgInstantiated(VarDecl):
  def __init__(self, genarg, type):
    super(GenericArgInstantiated, self).__init__( \
          ExprConstrained(ExprValue(genarg.name), typing.qbuiltin('nlang.meta.alias')), type)

class Expr(_FieldsEq):
  def __init__(self):
    super(Expr, self).__init__()
    self.maybeunarycall = False

  def nocache_typecheck(self, **ignored):
    raise Exception("Not implemented for type '%s'" % type(self))

  def itersubnodes(self, **kw):
    return iter('')

  def patterntypedestruct(self, xtype):
    return typing.checkcompat(self.typecheck(), xtype)

  def is_meta_type(self):
    return False

class ExprValue(Expr):
  def __init__(self, name):
    super(ExprValue, self).__init__()
    self.name = name

  def nocache_typecheck(self, **ignored):
    type = scope.current().q(self).typecheck()
    if isinstance(type, typing.TypeFunction):
      d = type.concrete_definition()
      if self.maybeunarycall and len(d.args) == 0:
        return d.rettype.typecheck()
      else:
        return type
    else:
      return type

  def is_meta_type(self):
    d = scope.current().q(self)
    return isinstance(d, TypeDef) or d.is_meta_type()

  def itersubnodes(self, **kw):
    d = self.definition()
    if self.maybeunarycall and isinstance(d, FunctionDecl):
      return _itersubnodes([UnaryCall(self)], **kw)
    else:
      return iter('')

  def typedestruct(self, genscope, t):
    '''t is the concrete type, self is the pattern'''
    # At this level, bottom of the pattern matching tree, t can be a more
    # complex type, so type(self) may be different from type(t).
    genarg = scope.current().rawq(self)
    if not isinstance(genarg, GenericArg):
      return

    if genarg.name not in genscope.table:
      genarg.setinstantiated(genscope, t)
    else:
      genarg.setinstantiated(genscope, typing.checkcompat(genscope.table[genarg.name].typecheck(), t))

  def patterntypedestruct(self, xtype):
    try:
      t = self.typecheck()
      return typing.checkcompat(t, xtype)
    except:
      # If unbound, this name is being defined.
      return xtype

  def __str__(self):
    return self.name

  def __deepcopy__(self, memo):
    global gdisable_instantiatedcopy
    global ginstantiatedcopy_sub
    if not gdisable_instantiatedcopy and self.name in ginstantiatedcopy_sub:
      cpy = ginstantiatedcopy_sub[self.name]
    else:
      cpy = ExprValue(self.name)
      cpy.maybeunarycall = self.maybeunarycall
    cpy.codeloc = self.codeloc
    cpy.codeloc.obj = cpy
    return cpy

  def declvars(self, expr):
    if self.name == '_':
      return []
    else:
      return [VarDecl(self, expr)]

class ExprRef(Expr):
  def __init__(self, access, value, nullable=False):
    super(ExprRef, self).__init__()
    if access == '@!':
      self.access = '!'
    elif access == '@':
      self.access = '.'
    else:
      self.access = access
    self.value = value
    self.setnullable(nullable)

  def setnullable(self, nullable):
    self.nullable = nullable

  def nocache_typecheck(self, **ignored):
    return typing.TypeRef(self.access, self.value.typecheck(), nullable=self.nullable)

  def definition(self):
    return self.deref(self.access).definition()

  def geninst_action(self):
    return False, False

  def itersubnodes(self, **kw):
    return _itersubnodes([self.value], **kw)

  def is_meta_type(self):
    return self.value.is_meta_type()

  def deref(self, access):
    if not self.is_meta_type() and self.nullable:
      raise errors.TypeError("Cannot dereference a nullable reference '%s', at %s" % (self, self.codeloc))
    if self.access != '!' and access == '!':
      raise errors.TypeError("Cannot mutate the type '%s', at %s" % (self, self.codeloc))
    return self.value

  def typedestruct(self, genscope, t):
    if not isinstance(t, typing.TypeRef):
      raise errors.PmStructError(self, t)
    if self.access != t.access and not (self.access == '.' and t.access == '!'):
      raise errors.PmStructError(self, t)
    if self.nullable != t.nullable and not (self.nullable and not t.nullable):
      raise errors.PmStructError(self, t)
    return self.value.typedestruct(genscope, t.type)

class ExprDeref(Expr):
  def __init__(self, access, value):
    super(ExprDeref, self).__init__()
    self.access = access
    self.value = value

  def nocache_typecheck(self, **ignored):
    return self.value.typecheck().deref(self.access)

  def itersubnodes(self, **kw):
    return _itersubnodes([self.value], **kw)

  def definition(self):
    return self.value.definition()

  def is_meta_type(self):
    return self.value.is_meta_type()

class ExprLiteral(Expr):
  def __init__(self, lit):
    super(ExprLiteral, self).__init__()
    self.args = [lit]

  def nocache_typecheck(self, **ignored):
    if isinstance(self.args[0], basestring):
      return typing.qbuiltin('nlang.literal.string')
    elif isinstance(self.args[0], bool):
      return typing.qbuiltin('nlang.literal.bool')
    else:
      return typing.qbuiltin('nlang.literal.integer')

  def patterntypedestruct(self, xtype):
    return typing.checkcompat(self.typecheck(), xtype)

  def declvars(self, expr):
    return [None]

  def definition(self):
    return self

class ExprNull(Expr):
  def __init__(self):
    super(ExprNull, self).__init__()

  def nocache_typecheck(self, **ignored):
    return typing.qbuiltin('nlang.literal.nulltype')

  def patterntypedestruct(self, xtype):
    return typing.checkcompat(self.typecheck(), xtype)

  def declvars(self, expr):
    return [None]

class ExprThis(Expr):
  def __init__(self):
    super(ExprThis, self).__init__()
    self.name = 'this'

  def nocache_typecheck(self, **ignored):
    return scope.current().q(self).typecheck()

  def is_meta_type(self):
    return True

class TupleInstance(_FieldsEq):
  def __init__(self, tuple):
    super(TupleInstance, self).__init__()
    self.tuple = tuple
    self.inscope = None

  def firstpass(self):
    ctx().gen_instances_fwd.add(self.tuple.typecheck())
    self._validate()

  def itersubnodes(self, **kw):
    return iter('')

class ExprTuple(_IsGenericInstance, Expr):
  def __init__(self, *args):
    super(ExprTuple, self).__init__()
    self.args = list(args)
    self.geninst = TupleInstance(self)

  def nocache_typecheck(self, **ignored):
    return typing.TypeTuple(*[t.typecheck() for t in self.args])

  def typedestruct(self, genscope, t):
    if not isinstance(t, typing.TypeTuple):
      raise errors.PmStructError(self, t)
    if len(self.args) != len(t.args):
      raise errors.PmStructError(self, t)
    for i in xrange(len(self.args)):
      return self.args[i].typedestruct(genscope, t.args[i])

  def patterntypedestruct(self, xtype):
    if not isinstance(xtype, typing.TypeTuple):
      raise errors.TypeError("Pattern matching a tuple '%s' to expression typed '%s', at %s" \
          % (self, xtype, self.codeloc))
    r = []
    for t, xt in zip(self.args, xtype.args):
      r.append(t.patterntypedestruct(xt))
    return typing.TypeTuple(*r)

  def itersubnodes(self, **kw):
    return _itersubnodes(self.args, **kw)

  def firstpass(self):
    super(ExprTuple, self).firstpass()
    self.geninst.firstpass()
    self._validate()

  def declvars(self, expr):
    r = []
    for i in xrange(len(self.args)):
      t = self.args[i]
      x = ExprTupleSelect(expr, i)
      r.extend(t.declvars(x))
    return r

  def is_meta_type(self):
    for a in self.args:
      if not a.is_meta_type():
        return False
    return True

class ExprTupleSelect(Expr):
  def __init__(self, expr, idx):
    super(ExprTupleSelect, self).__init__()
    self.expr = expr
    self.idx = idx

  def nocache_typecheck(self):
    return self.expr.typecheck().args[self.idx]

  def definition(self):
    return self.expr.typecheck().args[self.idx].concrete_definition()

class ExprConstrained(Expr):
  def __init__(self, expr, type):
    super(ExprConstrained, self).__init__()
    self.args = [expr]
    self.type = type

  def nocache_typecheck(self, **ignored):
    return typing.checkcompat(self.type.typecheck(), self.args[0].typecheck())

  def itersubnodes(self, **kw):
    return _itersubnodes(self.args, **kw)

  def patterntypedestruct(self, xtype):
    if xtype is None:
      return self.type.typecheck()
    else:
      return typing.checkcompat(self.type.typecheck(), xtype)

  def declvars(self, expr):
    r = self.args[0].declvars(expr)
    if len(r) == 1 and isinstance(r[0], VarDecl):
      r[0].type = self.type
    return r

  def is_meta_type(self):
    return self.type.typecheck() == typing.qbuiltin('nlang.meta.alias')

class ExprBin(Expr):
  def __init__(self, op, left, right):
    super(ExprBin, self).__init__()
    self.op = op
    self.args = [left, right]

  def nocache_typecheck(self, **ignored):
    return typing.unify(None, [t.typecheck() for t in self.args])

  def itersubnodes(self, **kw):
    return _itersubnodes(self.args, **kw)

  def definition(self):
    return None

class ExprCmpBin(ExprBin):
  def nocache_typecheck(self, **ignored):
    t = self.args[0].typecheck()
    typing.unify(None, [t.typecheck() for t in self.args])
    return typing.qbuiltin('nlang.numbers.bool')

class ExprBoolBin(ExprBin):
  def nocache_typecheck(self, **ignored):
    return typing.unify(typing.qbuiltin('nlang.numbers.bool'), [t.typecheck() for t in self.args])

class ExprIsa(ExprBin):
  def nocache_typecheck(self, **ignored):
    typing.unify(None, [t.typecheck() for t in self.args])
    return typing.qbuiltin('nlang.numbers.bool')

class ExprUnary(Expr):
  def __init__(self, op, expr):
    super(ExprUnary, self).__init__()
    self.op = op
    self.args = [expr]

  def nocache_typecheck(self, **ignored):
    return self.args[0].typecheck()

  def itersubnodes(self, **kw):
    return _itersubnodes(self.args, **kw)

  def definition(self):
    return None

class GenericInstance(_FieldsEq):
  def __init__(self, call):
    super(GenericInstance, self).__init__()
    self.call = call
    self.inscope = None
    self.ready = False
    self.defn = None

  def nocache_typecheck(self):
    return self.call.typecheck()

  def itersubnodes(self, **kw):
    return iter('')

  def definition(self):
    raise Exception('Do not call')

  def _call_definition(self):
    if not self.ready:
      return None
    else:
      return self.call.args[0].definition()

  def _instantiategeninst(self):
    if self.ready:
      return

    self.ready = True
    d = self._call_definition()
    if not d.unboundgeneric():
      self.defn = d
      return

    genscope = scope.Scope(self)

    if isinstance(d, FunctionDecl):
      if len(self.call.args) > 1 \
          and not isinstance(self.call.args[0], ExprSizeof) \
          and self.call.args[1].is_meta_type():

        # This ExprCall expression instantiate a generic function explicitly
        # eg: (fun T U) foo x:T y:T z:U = U
        #     in (foo u32 U8) 1 2 3

        for arg, term in zip(d.genargs, self.call.args[1:]):
          termtype = term.typecheck()
          with scope.push(d.scope):
            arg.typedestruct(genscope, termtype)

      else:
        for arg, term in zip(d.args, self.call.args[1:]):
          termtype = term.typecheck()
          with scope.push(d.scope):
            arg.type.typedestruct(genscope, termtype)

    else:
      for genarg, appliedarg in zip(d.type.args, self.call.args[1:]):
        argtype = appliedarg.typecheck()
        with scope.push(d.scope):
          genarg.typedestruct(genscope, argtype)

    self.defn = d.instantiated_copy(genscope)
    self.defn.firstpass()

    ctx().gen_instances_fwd.add(self.defn.typecheck())

class ExprCall(_IsGenericInstance, Expr):
  def __init__(self, fun, args):
    super(ExprCall, self).__init__()
    self.args = [fun] + args

    # This call may or may not be instantiating a generic.
    # But we don't know that yet.
    self.geninst = GenericInstance(self)

  def itersubnodes(self, **kw):
    for n in _itersubnodes(self.args, **kw):
      yield n
    for n in _itersubnodes([self.geninst._call_definition()], **kw):
      yield n

  def firstpass(self):
    for n in _itersubnodes(self.args, onelevel=True):
      n.firstpass()

    self.geninst._instantiategeninst()
    self._validate()

  def definition(self):
    d = self.geninst.defn
    if d is None:
      d = self.args[0].concrete_definition()

    if isinstance(d, FunctionDecl):
      if len(self.args) > 1 \
          and not isinstance(self.args[0], ExprSizeof) \
          and self.args[1].is_meta_type():
        # This ExprCall expression instantiates a generic function explicitly
        # eg: (foo u32 U8) 1 2 3
        return d
      else:
        return d.rettype.definition()

    elif isinstance(d, ChoiceDecl):
      return d.typedecl
    else:
      return d.definition()

  def nocache_typecheck(self, **ignored):
    d = self.geninst.defn
    if d is None:
      d = self.args[0].definition()
    assert not d.unboundgeneric() and "REALLY? also, make sure (accordion t) is correctly typedestruct?" != ""

    if isinstance(d, FunctionDecl):
      if len(self.args) > 1 \
          and not isinstance(self.args[0], ExprSizeof) \
          and self.args[1].is_meta_type():
        # This ExprCall expression instantiates a generic function explicitly
        # eg: (foo u32 U8) 1 2 3
        return self.geninst.defn.typecheck()

      with scope.push(d.scope):
        for a in d.args:
          a.typecheck()
        argtypes = [a.typecheck() for a in d.args]
        rettype = d.rettype.typecheck()

      for a,b in zip(argtypes, self.args[1:]):
        typing.checkcompat(a, b.typecheck())

      return rettype

    elif isinstance(d, ChoiceDecl):
      if len(self.args) > 2:
        raise errors.TypeError("Too many arguments to '%s', at %s" % (d, self.codeloc))

      with scope.push(d.scope):
        return d.defn.typecheck()

    else:
      return d.typecheck()

  def is_meta_type(self):
    if len(self.args) > 1 and not isinstance(self.args[0], ExprSizeof):
      for a in self.args[1:]:
        if not a.is_meta_type():
          return False
      return True
    else:
      return False

  def typedestruct(self, genscope, t):
    if isinstance(t, typing.TypeRef):
      raise errors.PmStructError(self, t)

    td = t.concrete_definition()
    if str(self.definition().scope) != str(td.scope):  # Test if refer to different type definitions
      found = False
      for i in td.listisa:
        try:
          with scope.push(t.genscope):
            with scope.push(td.scope):
              ti = i.typecheck()
          self.typedestruct(genscope, ti)
          found = True
          break
        except errors.PmStructError, ignore:
          continue
      if not found:
        raise errors.PmStructError(self, t)
    for u,v in zip(self.args[1:], t.args):
      u.typedestruct(genscope, v)

  def patterntypedestruct(self, xtype):
    t = self.typecheck()
    xd = xtype.concrete_definition()
    if isinstance(xd, TypeDecl) and xd.kind != TypeDecl.TAGGEDUNION:
      raise errors.TypeError("Pattern matching a type application '%s' to expression typed '%s', at %s" \
          % (self, xtype, self.codeloc))

    choice = xd.choicedecl(self.args[0])
    self.args[1].patterntypedestruct(choice.typearg.typecheck())

    return typing.checkcompat(self.typecheck(), xtype)

  def declvars(self, expr):
    t = self.args[1]
    x = ExprChoiceArgSelect(self.args[0], expr)
    return t.declvars(x)

class ExprChoiceArgSelect(Expr):
  def __init__(self, choice, expr):
    super(ExprChoiceArgSelect, self).__init__()
    self.choice = choice
    self.expr = expr

  def nocache_typecheck(self):
    return self.expr.concrete_definition() \
        .choicedecl(self.choice).typearg.typecheck()

  def definition(self):
    return self.expr.definition() \
        .choicedecl(self.choice).typearg.definition()

class ExprTypeApp(ExprCall, _NameEq):
  def __init__(self, type, *args):
    super(ExprTypeApp, self).__init__(type, list(args))
    self.geninst = GenericInstance(self)

  def is_meta_type(self):
    return True

class ExprTypeSlice(ExprTypeApp):
  def __init__(self, type):
    super(ExprTypeSlice, self).__init__(ExprValue('slice'), type)

class UnaryCall(ExprCall):
  def __init__(self, fun):
    super(UnaryCall, self).__init__(fun, [])

  def itersubnodes(self, **kw):
    return _itersubnodes(self.args[1:], **kw)

  def firstpass(self):
    self.geninst._instantiategeninst()
    self._validate()

class ExprField(Expr):
  def __init__(self, container, access, field):
    super(ExprField, self).__init__()
    self.container = container
    self.access = access
    self.field = field

  def nocache_typecheck(self, **ignored):
    d = self.definition()
    if self.maybeunarycall and isinstance(d, FunctionDecl):
      with scope.push(d.scope):
        return d.rettype.typecheck()
    else:
      return scope.current().q(self).typecheck()

  def patterntypedestruct(self, xtype):
    return typing.checkcompat(self.typecheck(), xtype)

  def itersubnodes(self, **kw):
    d = self.definition()
    if self.maybeunarycall and isinstance(d, FunctionDecl):
      with scope.push(d.scope):
        return _itersubnodes(UnaryCall(self), **kw)
    else:
      return iter('')

  def itersubnodes(self, **kw):
    return _itersubnodes([self.container], **kw)

  def __str__(self):
    return str(self.container) + self.access + str(self.field)

  def is_meta_type(self):
    d = scope.current().q(self)
    if isinstance(d, TypeDef):
      return not isinstance(d, FunctionDecl)
    else:
      return d.is_meta_type()

class ExprFieldElement(ExprCall):
  def __init__(self, container, access, idxexpr):
    super(ExprFieldElement, self).__init__(ExprField(container, access, ExprValue('operator_get__')), [idxexpr])

  def is_meta_type(self):
    return False

class ExprInitStaticConstField(Expr):
  """Special interal expr for initializing a global static"""
  def __init__(self, field_name, expr):
    super(ExprInitStaticConstField, self).__init__()
    self.field_name = field_name
    self.expr = expr

class ExprSizeof(ExprField):
  def __init__(self):
    super(ExprSizeof, self).__init__(
        ExprField(ExprValue('nlang'), '.', ExprValue('prelude')),
        '.', ExprValue('_sizeof'))

  def is_meta_type(self):
    return False

class ExprInitializer(Expr):
  def __init__(self, expr, pairs):
    super(ExprInitializer, self).__init__()
    self.expr = expr
    self.pairs = pairs

  def nocache_typecheck(self, **ignored):
    d = self.expr.concrete_definition()
    for name, expr in self.pairs:
      typing.checkcompat(d.scope.q(_QueryWrapper(name)).typecheck(), expr.typecheck())
    return self.expr.typecheck()

  def definition(self):
    return self.expr.definition()

  def itersubnodes(self, **kw):
    return _itersubnodes([self.expr] + [x for _,x in self.pairs], **kw)

  def is_meta_type(self):
    return False

class ExprAssign(_FieldsEq):
  def __init__(self, value, expr):
    super(ExprAssign, self).__init__()
    self.value = value
    self.expr = expr

  def nocache_typecheck(self, **ignored):
    typing.checkcompat(self.value.typecheck(), self.expr.typecheck())
    return typing.qbuiltin('nlang.numbers.void')

  def itersubnodes(self, **kw):
    return _itersubnodes([self.value, self.expr], **kw)

  def is_meta_type(self):
    return False

class ExprReturn(_FieldsEq):
  def __init__(self, expr):
    super(ExprReturn, self).__init__()
    self.expr = expr

  def nocache_typecheck(self, **ignored):
    if self.expr is None:
      return typing.qbuiltin('nlang.numbers.void')
    else:
      return self.expr.typecheck()

  def itersubnodes(self, **kw):
    return _itersubnodes([self.expr], **kw)

  def is_meta_type(self):
    return False

class ExprContinue(_FieldsEq):
  def nocache_typecheck(self, **ignored):
    return typing.qbuiltin('nlang.numbers.void')

  def itersubnodes(self, **kw):
    return iter('')

  def is_meta_type(self):
    return False

class ExprBreak(_FieldsEq):
  def nocache_typecheck(self, **ignored):
    return typing.qbuiltin('nlang.numbers.void')

  def itersubnodes(self, **kw):
    return iter('')

  def is_meta_type(self):
    return False

class Pass(_FieldsEq):
  def nocache_typecheck(self, **ignored):
    return typing.qbuiltin('nlang.numbers.void')

  def itersubnodes(self, **kw):
    return iter('')

  def is_meta_type(self):
    return False

class Assert(_FieldsEq):
  def __init__(self, expr):
    super(Assert, self).__init__()
    self.expr = expr

  def nocache_typecheck(self, **ignored):
    self.expr.typecheck()
    return typing.qbuiltin('nlang.numbers.void')

  def itersubnodes(self, **kw):
    return _itersubnodes([self.expr], **kw)

  def is_meta_type(self):
    return False

class ExprWhile(_FieldsEq, Decl):
  def __init__(self, cond, body):
    super(While, self).__init__()
    self.cond = cond
    self.body = body
    self._fillscope()

  def _fillscope(self):
    self.scope = scope.Scope(self)
    if self.body is not None:
      self.scope.define(self.body)

  def nocache_typecheck(self, **ignored):
    typing.checkcompat(typing.qbuiltin('nlang.numbers.bool'), self.cond.typecheck())
    self.body.typecheck()
    return typing.qbuiltin('nlang.numbers.void')

  def itersubnodes(self, **kw):
    return _itersubnodes([self.cond, self.body], **kw)

  def is_meta_type(self):
    return False

class ExprFor(_FieldsEq, Decl):
  def __init__(self, vardecl, iter, body):
    super(ExprFor, self).__init__()
    self.vardecl = vardecl
    self.iter = iter
    self.body = body
    self._fillscope()

  def _fillscope(self):
    self.scope = scope.Scope(self)
    self.scope.define(self.vardecl)
    if self.body is not None:
      self.scope.define(self.body)

  def nocache_typecheck(self, **ignored):
    ## FIXME: Validate the compatibility of vardecl and iter.
    ## FIXME: Infer type of vardecl from iter.
    self.vardecl.typecheck()
    self.iter.typecheck()
    self.body.typecheck()
    return typing.qbuiltin('nlang.numbers.void')

  def itersubnodes(self, **kw):
    return _itersubnodes([self.vardecl, self.iter, self.body], **kw)

  def is_meta_type(self):
    return False

class ExprPFor(ExprFor):
  pass

class ExprIf(_FieldsEq, Decl):
  def __init__(self, condpairs, elsebody):
    super(ExprIf, self).__init__()
    self.condpairs = condpairs
    self.elsebody = elsebody
    self._fillscope()

  def _fillscope(self):
    self.scope = scope.Scope(self)
    for _, b in self.condpairs:
      self.scope.define(b)
    if self.elsebody is not None:
      self.scope.define(self.elsebody)

  def nocache_typecheck(self, **ignored):
    typing.unify(typing.qbuiltin('nlang.numbers.bool'), [c.typecheck() for c,_ in self.condpairs])
    bodies = [b.typecheck() for _,b in self.condpairs]
    if self.elsebody is not None:
      bodies.append(self.elsebody.typecheck())
    t = typing.unify(None, bodies)
    if t != typing.qbuiltin('nlang.numbers.void'):
      # FIXME: Make sure the conditions are exhaustive, when applicable. In which
      # case we do not need a else.
      if self.elsebody is None:
        return typing.qbuiltin('nlang.numbers.void')
    return t

  def itersubnodes(self, **kw):
    a, b = zip(*self.condpairs)
    return _itersubnodes(list(a) + list(b) + [self.elsebody], **kw)

  def is_meta_type(self):
    return False

class ExprBlock(_FieldsEq, Decl):
  def __init__(self, body):
    super(ExprBlock, self).__init__()
    self.body = body
    self.main = False
    self._fillscope()

  def _fillscope(self):
    self.scope = scope.Scope(self)
    for d in self.body:
      if isinstance(d, Decl):
        self.scope.define(d)

  def nocache_typecheck(self, **ignored):
    if len(self.body) == 0:
      return typing.qbuiltin('nlang.numbers.void')
    else:
      with scope.push(self.scope):
        for b in self.body:
          b.typecheck()
        return self.body[-1].typecheck(statement=True)

  def itersubnodes(self, **kw):
    return _itersubnodes(self.body, **kw)

  def is_meta_type(self):
    return False

class ExprFuture(ExprBlock):
  pass

class ExprMatcher(_FieldsEq, Decl):
  def __init__(self, pattern, body):
    self.pattern = pattern
    self.body = body
    self.match = None
    self.codetmp = ExprValue(gensym())
    self.vars = [VarDecl(ExprConstrained(self.codetmp, ExprValue('void')), None)] \
        + self.pattern.declvars(self.codetmp)
    self.scope = scope.Scope(self)
    self.scope.define(self.body)
    for v in self.vars:
      self.scope.define(v)

  def patterntypecheck(self):
    xtype = self.match.expr.typecheck()
    with scope.push(self.match.patternscope()):
      return self.pattern.patterntypedestruct(xtype)

  def firstpass(self):
    self.vars[0].type = self.patterntypecheck()
    for n in self.itersubnodes(onelevel=True):
      n.firstpass()
    self._validate()

  def itersubnodes(self, **kw):
    return _itersubnodes([self.body] + self.vars, **kw)

class ExprMatch(_FieldsEq, Decl):
  def __init__(self, expr, matchers):
    self.expr = expr
    self.exprevaltmp = ExprValue(gensym())
    self.varexprevaltmp = VarDecl(self.exprevaltmp, self.expr)
    self.matchers = matchers
    for m in self.matchers:
      m.match = self
      m.vars[0].expr = self.exprevaltmp

    self.scope = scope.Scope(self)
    self.scope.define(self.varexprevaltmp)
    for m in self.matchers:
      self.scope.define(m)

  def itersubnodes(self, **kw):
    return _itersubnodes([self.expr] + self.matchers, **kw)

  def patternscope(self):
    xd = self.expr.concrete_definition()
    if isinstance(xd, TypeDecl) \
        and (xd.kind == TypeDecl.TAGGEDUNION \
             or xd.kind == TypeDecl.ENUM):
      return xd.scope
    else:
      return None

  def is_meta_type(self):
    return False

class SemanticAssert(_FieldsEq):
  def __init__(self, expr):
    super(SemanticAssert, self).__init__()
    self.expr = expr

  def itersubnodes(self, **kw):
    return iter('')

class SemanticClaim(_FieldsEq):
  def __init__(self, expr):
    super(SemanticClaim, self).__init__()
    self.expr = expr

  def itersubnodes(self, **kw):
    return iter('')

class Import(_NameEq):
  def __init__(self, path, all=False, alias=None):
    if alias is not None:
      self.modname = '.'.join(path[:-1])
    else:
      self.modname = '.'.join(path)
    super(Import, self).__init__(path[-1])
    self.path = path
    self.all = all
    self.alias = alias
    self.owner = None

  def allnames(self):
    sc = scope.current()
    container = sc.q(_QueryWrapper(self.modname))
    if self.all:
      return container.exportables()
    else:
      return [container.scope.q(im.path[-1])]

  def itersubnodes(self, **kw):
    return _itersubnodes(self.allnames(), **kw)

gmodctx = {}
gmodname = []

def ctx():
  global gmodctx
  global gmodname
  return gmodctx[gmodname[-1]]

class ModuleContext(object):
  def __init__(self, modname, fn):
    self.modname = modname
    self.fn = fn
    self.line = 1

    self.gen_instances_fwd = set()
    self.importednames = set()

class PlaceholderModule(_NameEq):
  '''When importing module 'a.b.c', modules 'a' and 'a.b' are not imported.
  However, we need something to return when looking up 'a', or 'a.b' in the
  current scope (in which 'a.b.c' has been imported). A PlaceholderModule
  is returned. If, later, 'a' is imported, then the PlaceholderModule is
  replaced with the actual Module 'a' and the scope of the PlaceholderModule
  (that contains a reference to 'a.b') is copied over.
  '''

  def __init__(self, name):
    # If name is None, this is the root.
    super(PlaceholderModule, self).__init__(name)
    self.scope = scope.Scope(self)

  def nocache_typecheck(self):
    return typing.Type(self)

  def definition(self):
    return self

class Module(_NameEq):
  def __init__(self, defs):
    super(Module, self).__init__('<anonymous>')
    self.imports = []
    self.imported = []
    self.toplevels = []
    for d in defs:
      if isinstance(d, basestring) and d == '\n':
        pass
      elif isinstance(d, Import):
        self.imports.append(d)
        d.owner = self
      else:
        self.toplevels.append(d)
    self._fillscope()
    self.ctx = None

  def _fillscope(self):
    self.scope = scope.Scope(self)
    for x in self.toplevels:
      self.scope.define(x)

  def setname(self, name, filename=None):
    self.path = name.split('.')
    self.name = self.path[-1]
    self.fullname = name
    self.filename = filename

    root = PlaceholderModule('<root>')
    s = root.scope
    for i in xrange(len(self.path)):
      p = self.path[i]
      if p in s.table:
        s = s.table[p].scope
      else:
        if i < len(self.path) - 1:
          ph = PlaceholderModule(p)
          s.table[p] = ph
          ph.scope.parent = s
          s = ph.scope
        else:
          s.table[p] = self
          self.scope.parent = s

  def exportables(self):
    return self.imported + self.toplevels

  def itersubnodes(self, **kw):
    return _itersubnodes(self.toplevels, **kw)

  def nocache_typecheck(self):
    return typing.Type(self)

  def definition(self):
    return self
