import re
import errors
import scope
import parser
import typing

def _deepstr(x):
  if isinstance(x, dict):
    s = '{ '
    for k, v in x.iteritems():
      s += "'%s': %s, " % (k, _deepstr(v))
    return s + '}'
  elif isinstance(x, tuple):
    s = '('
    for v in x:
      s += _deepstr(v) + ', '
    return s + ')'
  elif isinstance(x, list):
    s = '['
    for v in x:
      s += _deepstr(v) + ', '
    return s + ']'
  elif isinstance(x, basestring):
    return '"' + x + '"'
  elif isinstance(x, scope.Scope):
    return str(x)
  elif hasattr(x, '__dict__'):
    s = x.__class__.__name__ + '{ '
    for k, v in x.__dict__.iteritems():
      s += "'%s': %s, " % (k, _deepstr(v))
    return s + '}'
  else:
    return str(x)

def _listwrap(x):
  if isinstance(x, list):
    return x
  else:
    return [x]

class _NameEq(object):
  def __init__(self, name, scope=None):
    self.name = name
  def __str__(self):
    return self.name
  def __hash__(self):
    return hash(self.name)
  def __eq__(self, other):
    return self.name == other.name
  def __ne__(self, other):
    return self.name != other.name

class _FieldsEq(object):
  def __str__(self):
    return _deepstr(self)
  def __hash__(self):
    return hash(str(self))
  def __eq__(self, other):
    return str(self) == str(other)
  def __ne__(self, other):
    return str(self) != str(other)

class CGlobalName(object):
  pass

class Type(_NameEq, CGlobalName):
  def __init__(self, name):
    super(Type, self).__init__(Type.normalize(name))
  @staticmethod
  def normalize(name):
    return re.sub(r'\s*,\s+', ', ', re.sub(r'\s+', ' ', name))
  def deref(self, access):
    raise TypeError("Cannot dereference type '%s'" % self.name)
  def typecheck(self):
    return self

class TypeRef(Type):
  def __init__(self, access, type):
    super(TypeRef, self).__init__(access + type.name)
    self.access = access
    self.type = type
  def deref(self, access):
    if self.access != '!' and access == '!':
      raise TypeError("Cannot mutate the type '%s'" % self)
    return self.type

class TypeRefNullable(TypeRef):
  pass

class TypeTuple(Type):
  def __init__(self, *types):
    super(TypeTuple, self).__init__(', '.join([t.name for t in types]))
    self.types = types
    global gmodctx
    global gmodname
    gmodctx[gmodname].tuples.add(TupleDecl(self))

class TypeApp(Type):
  def __init__(self, type, *args):
    super(TypeApp, self).__init__(' '.join([t.name for t in [type] + list(args)]))
    self.type = type
    self.args = args

    global gctx
    global gmodname
    modtypeapps = gctx[gmodname].typeapps
    if self.type not in modtypeapps:
      modtypeapps[self.type] = {}
    if self not in modtypeapps[self.type]:
      modtypeapps[self.type][self] = TypeAppDecl(self)
    self.typeappdecl = modtypeapps[self.type][self]

class TypeGeneric(Type):
  def __init__(self, type, *args):
    super(TypeGeneric, self).__init__(type.name)
    self.type = type
    self.args = list(args)

class TypeGenericArg(Type):
  def __init__(self, name):
    super(TypeGenericArg, self).__init__(name)
    self.instantiated = None

class TypeSlice(TypeApp):
  def __init__(self, typeref):
    super(TypeSlice, self).__init__(Type('Slice'), typeref)

class ChoiceDecl(_NameEq, CGlobalName):
  def __init__(self, choice, typearg=None):
    if isinstance(choice, Assign):
      self.name = choice.value
      self.value = choice.expr
    else:
      self.name = choice
      self.value = None
    self.typearg = typearg

class TypeDecl(_NameEq, CGlobalName):
  REC, TAGGEDUNION, UNION, FORWARD = range(4)
  def __init__(self, type, isa, imports, typedecls, decls, methods, funs):
    super(TypeDecl, self).__init__(type.name)
    self.type = type
    self.isa = isa
    self.imports = imports
    self.typedecls = typedecls
    self.decls = decls
    self.kind = TypeDecl.whatkind(decls)
    self.methods = methods
    self.funs = funs
    self._fillscope()

  def _fillscope(self):
    self.scope = scope.Scope(self)
    if isinstance(self.type, TypeGeneric):
      for a in self.type.args:
        self.scope.define(a)
    for a in self.isa + self.imports + self.typedecls:
      self.scope.define(a)
    for a in self.decls:
      self.scope.define(a, name=a.name)

    for a in self.methods:
      self.scope.define(a, name=a.name)
      a.scope.define(VarDecl('this', TypeRef(a.access, self)))
      a.scope.define(Type(self.name), name='This')

    for a in self.funs:
      self.scope.define(a, name=a.name)
      a.scope.define(Type(self.name), name='This')

  @staticmethod
  def whatkind(decls):
    if not isinstance(decls, list):
      decls = [decls]
    if len(decls) == 0:
      return TypeDecl.FORWARD

    k = None
    for d in decls:
      if isinstance(d, ChoiceDecl):
        kind = TypeDecl.TAGGEDUNION
      else:
        kind = TypeDecl.REC

      if k is None:
        k = kind
      elif k != kind:
        raise errors.ParseError("Type declaration must have uniform kind")

    return k

  @classmethod
  def extract(cls, t, args):
    if args is None:
      return []
    r = []
    for a in args:
      if isinstance(a, list):
        r.extend(cls.extract(t, a))
      elif type(a) == t:
        r.append(a)
    return r

class TupleDecl(TypeDecl):
  def __init__(self, type):
    super(TypeDecl, self).__init__(type.name)
    self.type = type

class TypeAppDecl(TypeDecl):
  def __init__(self, typeapp):
    super(TypeDecl, self).__init__(typeapp.name)
    self.typeapp = typeapp
    self.typedecl = None

class Union(TypeDecl):
  pass

class FunctionDecl(_NameEq, CGlobalName):
  def __init__(self, name, args, returns, body):
    self.name = name
    self.args = args
    self.returns = returns
    self.body = body
    if len(self.returns) > 1:
      self.rettype = TypeTuple(*self.returns)
    else:
      self.rettype = self.returns[0]
    self._fillscope()
    self.type = self

  def _fillscope(self):
    self.scope = scope.Scope(self)
    for a in self.args:
      if isinstance(a, VarDecl):
        self.scope.define(a)
    for a in self.returns:
      if isinstance(a, VarDecl):
        self.scope.define(a)
    for a in self.body:
      if isinstance(a, VarDecl):
        self.scope.define(a)

  @classmethod
  def doparse(cls, args):
    assert isinstance(args, list)
    name = args[1]
    if isinstance(args[2], basestring) and args[2] == '=':
      n = 3
      funargs = []
    else:
      n = 4
      funargs = _listwrap(args[2])
    returns = _listwrap(args[n])
    body = []
    if n+1 < len(args):
      body = _listwrap(args[n+1])
    return cls(name, funargs, returns, body)

class MethodDecl(FunctionDecl):
  def __init__(self, name, access, args, returns, body):
    self.access = access
    super(MethodDecl, self).__init__(name, args, returns, body)

class VarDecl(_NameEq):
  def __init__(self, name, type, expr=None):
    super(VarDecl, self).__init__(name)
    self.type = type
    self.expr = expr
    self.mutatingblock = None
    self.scope = scope.Scope(self)
  def typecheck(self):
    return self.type

class FieldDecl(VarDecl):
  pass

class Expr(_FieldsEq):
  def typecheck(self):
    raise Exception("Not implemented for type '%s'" % type(self))

class Value(Expr):
  def __init__(self, name):
    self.name = name
  def typecheck(self):
    return scope.current().q(self).type

class Ref(Expr):
  def __init__(self, access, value):
    if access == '&!':
      self.access = '!'
    else:
      self.access = '.'
    self.value = value
  def typecheck(self):
    return TypeRef(self.access, self.value.typecheck())

class Deref(Expr):
  def __init__(self, access, value):
    self.access = access
    self.value = value
  def typecheck(self):
    return self.value.typecheck().deref(self.access)

class ValueField(Expr):
  def __init__(self, container, access, field):
    self.name = container
    self.access = access
    self.field = field
  def typecheck(self):
    return scope.current().q(self).type

class ExprLiteral(Expr):
  def __init__(self, lit):
    self.terms = [lit]
  def typecheck(self):
    if isinstance(self.terms[0], basestring):
      return Type('nlang.literal.String')
    else:
      return Type('nlang.literal.Integer')

class ExprNull(ExprLiteral):
  def __init__(self):
    pass
  def typecheck(self):
    return Type('nlang.literal.Null')

class ExprIdent(Expr):
  def __init__(self, ident):
    self.name = ident
    self.terms = []
  def typecheck(self):
    return scope.current().q(self).type

class Tuple(Expr):
  def __init__(self, *args):
    self.terms = list(args)
  def typecheck(self):
    return TypeTuple(*[t.typecheck() for t in self.terms])

class ConstrainedExpr(Expr):
  def __init__(self, expr, type):
    self.terms = [expr]
    self.type = type
  def typecheck(self):
    return typing.unify([self.typecheck(), self.terms[0].typecheck()])

class BinExpr(Expr):
  def __init__(self, op, left, right):
    self.op = op
    self.terms = [left, right]
  def typecheck(self):
    return typing.unify([t.typecheck() for t in self.terms])

class UnExpr(Expr):
  def __init__(self, op, expr):
    self.op = op
    self.terms = [expr]
  def typecheck(self):
    return typing.unify([t.typecheck() for t in self.terms])

class Call(Expr):
  def __init__(self, fun, args):
    self.terms = [fun] + args
  def typecheck(self):
    fun = self.terms[0]
    fun = scope.current().q(fun)
    if not isinstance(fun, FunctionDecl):
      raise TypeError("'%s' is not a function" % funname)
    return fun.rettype

class Assign(_FieldsEq):
  def __init__(self, value, expr):
    self.value = value
    self.expr = expr
  def typecheck(self):
    typing.checkcompat(self.value.typecheck(), self.expr.typecheck())

class Return(_FieldsEq):
  def __init__(self, expr):
    self.expr = expr

class Assert(_FieldsEq):
  def __init__(self, expr):
    self.expr = expr

class SemanticAssert(_FieldsEq):
  def __init__(self, expr):
    self.expr = expr

class SemanticClaim(_FieldsEq):
  def __init__(self, expr):
    self.expr = expr

class Import(_NameEq):
  def __init__(self, name, modname):
    super(Import, self).__init__(name)
    self.module = parser.parsemod(modname)

gctx = {}
gmodctx = {}
gmodname = None

class GlobalContext(object):
  def __init__(self):
    self.typeapps = {}

class ModuleContext(object):
  def __init__(self):
    self.tuples = set()

class Module(_NameEq):
  def __init__(self, defs):
    super(Module, self).__init__('<anonymous>')
    self.imports = []
    self.toplevels = []
    for d in defs:
      if isinstance(d, basestring) and d == '\n':
        pass
      elif isinstance(d, Import):
        self.imports.append(d)
      else:
        self.toplevels.append(d)
    self._fillscope()
    self.ctx = None

  def _fillscope(self):
    self.scope = scope.Scope(self)
    for x in self.imports:
      self.scope.define(x)
      if x.name != x.module.name:
        if x.name == '*':
          for d in x.module.imports + x.module.toplevels:
            self.scope.alias(d.name, x.module, d.name)
        else:
          self.scope.alias(x.name, x.module.name, x.name)
    for x in self.toplevels:
      self.scope.define(x)
