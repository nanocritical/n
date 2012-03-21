import re
import copy
import errors
import scope
import parser
import typing

gnextsym = 0

def gensym():
  global gnextsym
  n = gnextsym
  gnextsym += 1
  return '__nlang_gensym' + str(n) + '__'

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
  for a in args:
    if a is None:
      continue
    for g in a:
      if g is None:
        continue
      yield g
      if 'onelevel' in kw and kw['onelevel']:
        continue
      for sub in g.itersubnodes(**kw):
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

  def fixscope(self):
    if hasattr(self, 'scope'):
      self.scope.parent_definition = self.scope.parent

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
        if descend:
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
      #for geninst in self.itergeninsts(onelevel=True):
      #  geninst.firstpass()

  def typecheck(self, *args, **kw):
    if self.cachedtype is None:
      type = self.nocache_typecheck(*args, **kw)
      assert isinstance(type, typing.Typename)
      if not isinstance(type, typing.TypeUnboundGeneric):
        self.cachedtype = type
      return type
    else:
      return self.cachedtype

  def definition(self):
    return scope.current().q(self).definition()

  def concrete_definition(self):
    return self.typecheck().concrete_definition()

  def unboundgeneric(self):
    return False

class _IsGenericInstance(object):
  def geninst_action(self):
    return True, True

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
    print 'genarg!!!!!!!', self.codeloc
    return self.instantiated()

  def definition(self):
    return None

  def __deepcopy__(self, memo):
    global ginstantiatedcopy_sub
    if self.name in ginstantiatedcopy_sub:
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

class Decl(object):
  pass

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
      if hasattr(self, 'imports'):
        for im in self.imports:
          imsrcs = _imported_sources(im)
          self.imported.append(imsrcs)
      for n in self.itersubnodes(onelevel=True):
        n.firstpass()

      #for geninst in self.itergeninsts(onelevel=True):
      #  geninst.firstpass()

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
  def __init__(self, type, genargs, listisa, imports, typedecls, decls, methods, funs):
    self.type = type
    self.genargs = genargs
    super(Intf, self).__init__(type.name)
    self.listisa = listisa
    self.imports = imports
    self.imported = []
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
    return self.imported + self.typedecls + self.decls + self.funs + self.methods
  def itersubnodes(self, **kw):
    return _itersubnodes(self.listisa, self.imports, self.typedecls, self.decls, self.methods, self.funs, **kw)

class DynIntf(TypeDef, Decl):
  def __init__(self, type, genargs, listisa, imports, typedecls, methods):
    self.type = type
    self.genargs = genargs
    super(DynIntf, self).__init__(type.name)
    self.listisa = listisa
    self.imports = imports
    self.imported = []
    self.typedecls = typedecls
    self.methods = methods
    self._fillscope()

  def _fillscope(self):
    self.scope = scope.Scope(self)
    if isinstance(self.type, GenericTypename):
      for a in self.type.args:
        self.scope.define(a)
    self.scope.define(self.type, name='this')
    for a in self.listisa + self.imports + self.typedecls:
      self.scope.define(a)
    for a in self.methods:
      self.scope.define(a)

  def exportables(self):
    return self.imported + self.typedecls + self.decls + self.methods
  def itersubnodes(self, **kw):
    return _itersubnodes(self.listisa, self.imports, self.typedecls, self.decls, self.methods, **kw)

class ChoiceDecl(TypeDef, CGlobalName):
  def __init__(self, choice, typearg=None):
    super(ChoiceDecl, self).__init__(choice)
    self.value = None
    self.typearg = typearg
    self.defn = None

  def _defbuiltins(self):
    if self.typearg is not None:
      args = [VarDecl(ExprConstrained(ExprValue('arg'), self.typearg))]
    else:
      args = []
    init = ExprInitializer(self.defn.type,
        [('which', ExprLiteral(self.value))])
    self.mk = FunctionDecl('Mk', [], args, [self.defn.type], ExprBlock([ExprReturn(init)]))
    self.valuevar = FieldConstDecl( \
        VarDecl(ExprConstrained(ExprValue('Value'), ExprValue('U32')), \
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

class TypeDecl(TypeDef, Decl, CGlobalName):
  REC, TAGGEDUNION, ENUM, UNION, FORWARD = range(5)

  def __init__(self, type, genargs, listisa, imports, typedecls, userdecls, methods, funs):
    self.type = type
    self.genargs = genargs  # Free type vars not in self.type.args
    super(TypeDecl, self).__init__(type.name)
    self.listisa = listisa
    self.imports = imports
    self.typedecls = typedecls

    self.kind = self.whatkind(userdecls)
    self.userdecls = userdecls
    self.decls = copy.copy(userdecls)
    self.declnum = None

    i = 0
    for d in userdecls:
      if isinstance(d, ChoiceDecl):
        d.defn = self
        d.value = i
        d._defbuiltins()
        i += 1
      elif isinstance(d, FieldDecl):
        d.typedecl = self
    if self.kind == TypeDecl.TAGGEDUNION:
      union = Union(ExprValue('__as'), [], [UnionField(d.name, d.typearg) for d in userdecls])
      self.typedecls.append(union)
      self.decls.append(FieldDecl(self, \
          ExprValue('__unsafe_as'), ExprField(ExprValue('this'), '.', ExprValue(union.name))))
    if self.kind == TypeDecl.TAGGEDUNION or self.kind == TypeDecl.ENUM:
      self.decls.append(FieldDecl(self, ExprValue('which'), ExprValue('U32')))
      self.declnum = FieldConstDecl( \
          VarDecl(ExprConstrained(ExprValue('NUM__'), ExprValue('U32')), ExprLiteral(len(userdecls))))

    self.methods = methods
    self.funs = funs
    self._fillscope()

  def _fillscope(self):
    self.scope = scope.Scope(self)
    genargs = []
    if isinstance(self.type, GenericTypename):
      for a in self.genargs:
        self.scope.define(a)
        genargs.append(a)
      for a in self.type.args:
        self.scope.define(a)
        genargs.append(a)
    self.scope.define(self.type, name='this')
    for a in self.imports + self.typedecls:
      self.scope.define(a)
    for a in self.decls:
      self.scope.define(a)
    self.scope.define(self.declnum)
    for a in self.methods:
      self.scope.define(a)
      a.scope.define(VarDecl(ExprConstrained( \
          ExprValue('self'), ExprTypeRef(a.access, self.type))))
      a.scope.define(self.type, name='this')
      for ga in genargs:
        a.scope.define(ga)
    for a in self.funs:
      self.scope.define(a)
      a.scope.define(self.type, name='this')
      for ga in genargs:
        a.scope.define(ga)

  def fixscope(self):
    super(TypeDecl, self).fixscope()
    # In the body of a method, current().q() is looking up at the module level.
    # I.e. class members are not in scope, they must be accessed via self, this
    # or an absolute path.
    for a in self.methods:
      a.scope.parent = self.scope.parent
    for a in self.funs:
      a.scope.parent = self.scope.parent

  def whatkind(self, decls):
    if not isinstance(decls, list):
      decls = [decls]
    if len(decls) == 0:
      return TypeDecl.FORWARD

    k = None
    for d in decls:
      if isinstance(d, ChoiceDecl):
        if d.typearg is None:
          kind = TypeDecl.ENUM
        else:
          kind = TypeDecl.TAGGEDUNION
      else:
        kind = TypeDecl.REC

      if k is None:
        k = kind
      elif k != kind:
        if k == TypeDecl.ENUM and kind == TypeDecl.TAGGEDUNION:
          k = kind
        elif k == TypeDecl.TAGGEDUNION and kind == TypeDecl.ENUM:
          pass
        else:
          raise errors.ParseError("Type declaration must have uniform kind, at %s" \
              % self.codeloc)

    return k

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
    return _itersubnodes(self.listisa, self.imports, self.typedecls, self.decls, self.funs, self.methods, **kw)

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

  def nocache_typecheck(self):
    if self.unboundgeneric():
      return typing.TypeUnboundGeneric(self)
    else:
      with scope.push(self.scope):
        return typing.TypeFunction(self, self.rettype.typecheck(), *[a.typecheck() for a in self.genargs])

  def itersubnodes(self, **kw):
    return _itersubnodes(self.args, self.returns, [self.rettype, self.body], **kw)

class MethodDecl(FunctionDecl):
  def __init__(self, name, genargs, access, args, returns, body):
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
    return self.type is not None and self.type.typecheck() == typing.qbuiltin('nlang.meta.Type')

  def nocache_typecheck(self, statement=False):
    t = self.rawtypecheck()
    if statement:
      return typing.qbuiltin('nlang.numbers.Void')
    else:
      return t

  def rawtypecheck(self):
    if self.type is not None:
      if self.expr is not None:
        if self.type.typecheck() == typing.qbuiltin('nlang.meta.Type'):
          # To support the expression: 'let typealias:nlang.meta.Type = SomeType'
          return self.expr.typecheck()
        else:
          typing.checkcompat(self.type.typecheck(), self.expr.typecheck())
      return self.type.typecheck()
    else:
      return self.expr.typecheck()

  def definition(self):
    if self.type is not None:
      if isinstance(self.type, typing.Typename):
        return self.expr.concrete_definition()
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
    self.vars = [VarDecl(ExprConstrained(self.codetmp, ExprValue('Void')), self.expr)]
    patternvars = self.pattern.declvars(self.codetmp)
    if len(patternvars) == 1:
      # Remove the unnecessary intermediate temporary.
      self.vars[0].name = patternvars[0].name
    else:
      self.vars.extend(patternvars)
    self.mutatingblock = None
    self.scope = scope.Scope(self)
    # Do not define the self.vars in self.scope: they need to be defined
    # in self.scope.parent.

  def is_meta_type(self):
    return isinstance(self.pattern, ExprConstrained) \
        and self.pattern.type.typecheck() == typing.qbuiltin('nlang.meta.Type')

  def patterntypecheck(self):
    if self.expr is None:
      return self.pattern.patterntypedestruct(None)
    else:
      xtype = self.expr.typecheck()
      return self.pattern.patterntypedestruct(xtype)

  def nocache_typecheck(self):
    return typing.qbuiltin('nlang.numbers.Void')

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

  def itersubnodes(self, **kw):
    # Do expr before vars.
    return _itersubnodes([self.expr, self.pattern, self.mutatingblock] + self.vars, **kw)

class FieldConstDecl(_NameEq, CGlobalName, Decl):
  def __init__(self, vardecl):
    super(FieldConstDecl, self).__init__(vardecl.name)
    self.vardecl = vardecl
    self.scope = scope.Scope(self)
    self.scope.define(vardecl)

  def itersubnodes(self, **kw):
    return _itersubnodes([self.vardecl], **kw)

  def nocache_typecheck(self, **ignored):
    return self.vardecl.typecheck()

  def definition(self):
    return self.vardecl.definition()

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
          ExprConstrained(ExprValue(genarg.name), typing.qbuiltin('nlang.meta.Type')), type)

class Expr(_FieldsEq):
  def __init__(self):
    super(Expr, self).__init__()
    self.maybeunarycall = False

  def nocache_typecheck(self, **ignored):
    raise Exception("Not implemented for type '%s'" % type(self))

  def itersubnodes(self, **kw):
    return iter('')

  def patterntypedestruct(self, xtype):
    return typing.unify([self.typecheck(), xtype])

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
    if self.maybeunarycall:
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
      genarg.setinstantiated(genscope, typing.unify([genscope.table[genarg.name].typecheck(), t]))

  def patterntypedestruct(self, xtype):
    try:
      t = self.typecheck()
      return typing.unify([t, xtype])
    except:
      # If unbound, this name is being defined.
      return xtype

  def __str__(self):
    return self.name

  def __deepcopy__(self, memo):
    global ginstantiatedcopy_sub
    if self.name in ginstantiatedcopy_sub:
      cpy = ginstantiatedcopy_sub[self.name]
    else:
      cpy = ExprValue(self.name)
    cpy.codeloc = self.codeloc
    cpy.codeloc.obj = cpy
    return cpy

  def declvars(self, expr):
    if self.name == '_':
      return []
    else:
      return [VarDecl(self, expr)]

class ExprRef(Expr):
  def __init__(self, access, value):
    super(ExprRef, self).__init__()
    if access == '&!':
      self.access = '!'
    else:
      self.access = '.'
    self.value = value

  def nocache_typecheck(self, **ignored):
    return typing.TypeRef(self.access, self.value.typecheck())

  def itersubnodes(self, **kw):
    return _itersubnodes([self.value], **kw)

  def is_meta_type(self):
    return self.value.is_meta_type()

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

class ExprTypeRef(Expr):
  def __init__(self, access, type, nullable=False):
    super(ExprTypeRef, self).__init__()
    self.access = access
    self.type = type
    self.setnullable(nullable)

  def setnullable(self, nullable):
    self.nullable = nullable

  def deref(self, access):
    if self.access != '!' and access == '!':
      raise errors.TypeError("Cannot mutate the type '%s', at %s" % (self, self.codeloc))
    return self.type

  def typedestruct(self, genscope, t):
    if not isinstance(t, typing.TypeRef):
      raise errors.PmStructError(self, t)
    if self.access != t.access and not (self.access == '.' and t.access == '!'):
      raise errors.PmStructError(self, t)
    if self.nullable != t.nullable and not (self.nullable and not t.nullable):
      raise errors.PmStructError(self, t)
    return self.type.typedestruct(genscope, t.type)

  def nocache_typecheck(self):
    resolved = self.type.typecheck()
    if self.type != resolved:
      return typing.TypeRef(self.access, resolved, nullable=self.nullable)
    else:
      # We return self in case this type doesn't depend on context
      # (generic arguments), to avoid creating redundant objects.
      return self

  def definition(self):
    return self.deref(self.access).definition()

  def geninst_action(self):
    return False, False

  def itersubnodes(self, **kw):
    return _itersubnodes([self.type], **kw)

  def is_meta_type(self):
    return True

class ExprLiteral(Expr):
  def __init__(self, lit):
    super(ExprLiteral, self).__init__()
    self.args = [lit]

  def nocache_typecheck(self, **ignored):
    if isinstance(self.args[0], basestring):
      return typing.qbuiltin('nlang.literal.String')
    elif isinstance(self.args[0], bool):
      return typing.qbuiltin('nlang.literal.Bool')
    else:
      return typing.qbuiltin('nlang.literal.Integer')

  def patterntypedestruct(self, xtype):
    return typing.unify([self.typecheck(), xtype])

  def declvars(self, expr):
    return [None]

  def definition(self):
    return self

class ExprNull(Expr):
  def __init__(self):
    super(ExprNull, self).__init__()

  def nocache_typecheck(self, **ignored):
    return typing.qbuiltin('nlang.literal.Null')

  def patterntypedestruct(self, xtype):
    return typing.unify([self.typecheck(), xtype])

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
    super(TupleInstance, self).firstpass()
    ctx().gen_instances_fwd.add(self.tuple.typecheck())

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
    return typing.unify([self.type.typecheck(), self.args[0].typecheck()])

  def itersubnodes(self, **kw):
    return _itersubnodes(self.args, **kw)

  def patterntypedestruct(self, xtype):
    if xtype is None:
      return self.type.typecheck()
    else:
      return typing.unify([self.type.typecheck(), xtype])

  def declvars(self, expr):
    r = self.args[0].declvars(expr)
    if len(r) == 1 and isinstance(r[0], VarDecl):
      r[0].type = self.type
    return r

  def is_meta_type(self):
    return self.type.typecheck() == typing.qbuiltin('nlang.meta.Type')

class ExprBin(Expr):
  def __init__(self, op, left, right):
    super(ExprBin, self).__init__()
    self.op = op
    self.args = [left, right]

  def nocache_typecheck(self, **ignored):
    return typing.unify([t.typecheck() for t in self.args])

  def itersubnodes(self, **kw):
    return _itersubnodes(self.args, **kw)

  def definition(self):
    return None

class ExprCmpBin(ExprBin):
  def nocache_typecheck(self, **ignored):
    t = self.args[0].typecheck()
    typing.unify([t.typecheck() for t in self.args])
    return typing.qbuiltin('nlang.numbers.Bool')

class ExprBoolBin(ExprBin):
  def nocache_typecheck(self, **ignored):
    return typing.unify([t.typecheck() for t in self.args] + [typing.qbuiltin('nlang.numbers.Bool')])

class ExprIsa(ExprBin):
  def nocache_typecheck(self, **ignored):
    typing.unify([t.typecheck() for t in self.args])
    return typing.qbuiltin('nlang.numbers.Bool')

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
        #     in (foo U32 U8) 1 2 3

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

  def definition(self):
    d = self.geninst.defn
    if d is None:
      d = self.args[0].concrete_definition()

    if isinstance(d, FunctionDecl):
      if len(self.args) > 1 \
          and not isinstance(self.args[0], ExprSizeof) \
          and self.args[1].is_meta_type():
        # This ExprCall expression instantiates a generic function explicitly
        # eg: (foo U32 U8) 1 2 3
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
    assert not d.unbound

    if isinstance(d, FunctionDecl):
      if len(self.args) > 1 \
          and not isinstance(self.args[0], ExprSizeof) \
          and self.args[1].is_meta_type():
        # This ExprCall expression instantiates a generic function explicitly
        # eg: (foo U32 U8) 1 2 3
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
    d = self.concrete_definition()
    if str(d.scope) != str(xtype.defn.scope):
      raise errors.TypeError("Pattern matching the type application '%s' to expression typed '%s', at %s" \
          % (d.scope, xtype, self.codeloc))

    choice = xd.choicedecl(self.args[0])
    self.args[1].patterntypedestruct(choice.typearg.typecheck())
    return typing.unify([self.typecheck(), xtype])

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
    super(ExprTypeSlice, self).__init__(ExprValue('Slice'), type)

class UnaryCall(ExprCall):
  def __init__(self, fun):
    super(UnaryCall, self).__init__(fun, [])

  def itersubnodes(self, **kw):
    return _itersubnodes(self.args[1:], **kw)

  def firstpass(self):
    self.geninst._instantiategeninst()

class ExprDtor(ExprCall):
  def __init__(self, type):
    super(ExprDtor, self).__init__(ExprField(type, '.', ExprValue('Dtor')), [])

class ExprField(Expr):
  def __init__(self, container, access, field):
    super(ExprField, self).__init__()
    self.container = container
    self.access = access
    self.field = field

  def nocache_typecheck(self, **ignored):
    d = self.definition()
    if self.maybeunarycall and isinstance(d, FunctionDecl):
      if hasattr(d.rettype, 'name') and d.rettype.name == 'this':  # FIXME hack
        return self.container.typecheck()
      else:
        return d.rettype.typecheck()
    else:
      return scope.current().q(self).typecheck()

  def patterntypedestruct(self, xtype):
    return typing.unify([self.typecheck(), xtype])

  def itersubnodes(self, **kw):
    d = self.definition()
    if self.maybeunarycall and isinstance(d, FunctionDecl):
      if hasattr(d.rettype, 'name') and d.rettype.name == 'this':  # FIXME hack
        pass
      else:
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
      d.is_meta_type()

class ExprFieldElement(ExprCall):
  def __init__(self, container, access, idxexpr):
    super(ExprFieldElement, self).__init__(ExprField(container, access, ExprValue('Get__')), [idxexpr])

  def is_meta_type(self):
    return False

class ExprSizeof(ExprField):
  def __init__(self):
    super(ExprSizeof, self).__init__(
          ExprField(ExprValue('nlang'), '.', ExprValue('prelude')),
          '.', ExprValue('Sizeof'))

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
    return typing.qbuiltin('nlang.numbers.Void')

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
      return typing.qbuiltin('nlang.numbers.Void')
    else:
      return self.expr.typecheck()

  def itersubnodes(self, **kw):
    return _itersubnodes([self.expr], **kw)

  def is_meta_type(self):
    return False

class ExprContinue(_FieldsEq):
  def nocache_typecheck(self, **ignored):
    return typing.qbuiltin('nlang.numbers.Void')

  def itersubnodes(self, **kw):
    return iter('')

  def is_meta_type(self):
    return False

class ExprBreak(_FieldsEq):
  def nocache_typecheck(self, **ignored):
    return typing.qbuiltin('nlang.numbers.Void')

  def itersubnodes(self, **kw):
    return iter('')

  def is_meta_type(self):
    return False

class Pass(_FieldsEq):
  def nocache_typecheck(self, **ignored):
    return typing.qbuiltin('nlang.numbers.Void')

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
    return typing.qbuiltin('nlang.numbers.Void')

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
    typing.checkcompat(self.cond.typecheck(), typing.qbuiltin('nlang.numbers.Bool'))
    self.body.typecheck()
    return typing.qbuiltin('nlang.numbers.Void')

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
    return typing.qbuiltin('nlang.numbers.Void')

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
    typing.unify([c.typecheck() for c,_ in self.condpairs] + [typing.qbuiltin('nlang.numbers.Bool')])
    bodies = [b.typecheck() for _,b in self.condpairs]
    if self.elsebody is not None:
      bodies.append(self.elsebody.typecheck())
    t = typing.unify(bodies)
    if t != typing.qbuiltin('nlang.numbers.Void'):
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
    self._fillscope()

  def _fillscope(self):
    self.scope = scope.Scope(self)
    for d in self.body:
      if isinstance(d, Decl):
        self.scope.define(d)

  def nocache_typecheck(self, **ignored):
    if len(self.body) == 0:
      return typing.qbuiltin('nlang.numbers.Void')
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
    self.vars = [VarDecl(ExprConstrained(self.codetmp, ExprValue('Void')), None)] \
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
    return iter('') #_itersubnodes([self.expr], **kw)

class SemanticClaim(_FieldsEq):
  def __init__(self, expr):
    super(SemanticClaim, self).__init__()
    self.expr = expr

  def itersubnodes(self, **kw):
    return iter('') #_itersubnodes([self.expr], **kw)

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

  def setname(self, name):
    self.path = name.split('.')
    self.name = self.path[-1]
    self.fullname = name

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
