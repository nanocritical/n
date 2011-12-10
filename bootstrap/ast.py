import re
import errors
import scope
import parser
import typing

def _shallowstr(x):
  if isinstance(x, dict):
    s = '{ '
    for k, v in x.iteritems():
      s += "'%s': %s, " % (k, v)
    return s + '}'
  elif isinstance(x, tuple):
    s = '('
    for v in x:
      s += _shallowstr(v) + ', '
    return s + ')'
  elif isinstance(x, list):
    s = '['
    for v in x:
      s += _shallowstr(v) + ', '
    return s + ']'
  elif isinstance(x, basestring):
    return '"' + x + '"'
  elif isinstance(x, scope.Scope):
    return str(x)
  elif hasattr(x, '__dict__'):
    s = x.__class__.__name__ + '{ '
    for k, v in x.__dict__.iteritems():
      s += "'%s': %s, " % (k, v)
    return s + '}'
  else:
    return str(x)

def _listwrap(x):
  if isinstance(x, list):
    return x
  else:
    return [x]

class CodeLoc(object):
  def __init__(self, obj):
    self.obj = obj
    global gmodname
    global gmodctx
    self.fn = gmodctx[gmodname[-1]].fn
    self.line = gmodctx[gmodname[-1]].line
  def __str__(self):
    s = self.fn + ':' + str(self.line)
    if 'name' in self.obj.__dict__:
      s += ': ' + self.obj.name
    return s

class _NameEq(object):
  def __init__(self, name, scope=None):
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

class _FieldsEq(object):
  def __init__(self):
    self.codeloc = CodeLoc(self)
  def __str__(self):
    return _shallowstr(self)
  def __hash__(self):
    return hash(str(self))
  def __eq__(self, other):
    return str(self) == str(other)
  def __ne__(self, other):
    return str(self) != str(other)

class CGlobalName(object):
  pass

class HasDep(object):
  '''For toplevel declarations, generic dependencies.

  Used to instantiate generics in the right place (after the declaration
  of the generic type, before the first use of the instantiation, but after
  the declaration of all the dependencies the instance uses.

  We only care about the additional ordering constraints coming from the
  generic parameters (as instantiated). The developer is in charge of
  the basic ordering of declarations, as in C.
  '''
  def dependson(self):
    raise Exception("Unimplemented for type '%s'" % type(self))

def _checkdeconstructcompat(x, y):
  if type(x) != type(y):
    raise errors.PmStructError(x, y)

class Type(_NameEq, CGlobalName):
  def __init__(self, name):
    super(Type, self).__init__(Type.normalize(name))
  @staticmethod
  def normalize(name):
    return re.sub(r'\s*,\s+', ', ', re.sub(r'\s+', ' ', name))
  def deref(self, access):
    raise errors.TypeError("Cannot dereference type '%s', at %s" % (self.name, self.codeloc))
  def typecheck(self):
    return self
  def typedef(self):
    return scope.current().q(self)
  def deconstruct(self, genscope, t):
    '''t is the concrete type, self is the pattern'''
    # At this level, bottom of the pattern matching tree, t can be a more
    # complex type, so type(self) may be different from type(t).
    genarg = genscope.rawq(self)
    if not isinstance(genarg, GenericArg):
      return

    if genarg.instantiated is None:
      genarg.instantiated = t
    else:
      genarg.instantiated = typing.unify([genarg.instantiated, t])

class TypeRef(Type):
  def __init__(self, access, type):
    super(TypeRef, self).__init__(access + type.name)
    self.access = access
    self.type = type
  def deref(self, access):
    if self.access != '!' and access == '!':
      raise errors.TypeError("Cannot mutate the type '%s', at %s" % (self, self.codeloc))
    return self.type
  def deconstruct(self, genscope, t):
    _checkdeconstructcompat(self, t)
    if self.access != t.access:
      # We may actually want to be tolerant here: does '.' matche '!' ?
      raise errors.PmStructError(self, t)
    return self.type.deconstruct(genscope, t.type)
  def typedef(self):
    return self.type.typedef()

class TypeRefNullable(TypeRef):
  def __init__(self, typeref):
    super(TypeRefNullable, self).__init__(typeref.access, typeref.type)

class TypeTuple(Type):
  def __init__(self, *types):
    super(TypeTuple, self).__init__(', '.join([t.name for t in types]))
    self.types = types

    self.tupledecl = TupleDecl(self)
    ctx().tuples_pending.append(self.tupledecl)

  def deconstruct(self, genscope, t):
    _checkdeconstructcompat(self, t)
    if len(self.types) != len(t.types):
      raise errors.PmStructError(self, t)
    for i in xrange(len(self.types)):
      return self.types[i].deconstruct(genscope, t.types[i])

class TypeApp(Type):
  def __init__(self, type, *args):
    super(TypeApp, self).__init__(' '.join([t.name for t in [type] + list(args)]))
    self.type = type
    self.args = args

    self.typeappdecl = TypeAppDecl(self)
    ctx().typeapps_pending.append(self.typeappdecl)

  def deconstruct(self, genscope, t):
    _checkdeconstructcompat(self, t)
    _checkdeconstructcompat(self.type, t.type)
    if len(self.type) != len(t.type):
      raise errors.PmStructError(self, t)
    for i in xrange(len(self.args)):
      return self.args[i].deconstruct(genscope, t.args[i])

class GenericTypename(Type):
  def __init__(self, type, *args):
    super(GenericTypename, self).__init__(type.name)
    self.type = type
    self.args = list(args)

class GenericArg(Type):
  def __init__(self, name):
    super(GenericArg, self).__init__(name)
    self.instantiated = None

class TypeSlice(TypeApp):
  def __init__(self, typeref):
    super(TypeSlice, self).__init__(Type('Slice'), typeref)

class Decl(object):
  pass

class TypeDef(object):
  def typedef(self):
    return self

class Intf(_NameEq, Decl, TypeDef):
  def __init__(self, type, isa, imports, typedecls, decls, methods, funs):
    super(Intf, self).__init__(type.name)
    self.type = type
    self.isa = isa
    self.imports = imports
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
    self.scope.define(self.type, name='This')
    for a in self.isa + self.imports + self.typedecls:
      self.scope.define(a)
    for a in self.decls:
      self.scope.define(a, name=a.name)
    for a in self.methods:
      self.scope.define(a, name=a.name)
    for a in self.funs:
      self.scope.define(a, name=a.name)

    ctx().acquire_pending(self.scope)

class DynIntf(_NameEq, Decl, TypeDef):
  def __init__(self, type, isa, imports, typedecls, methods):
    super(DynIntf, self).__init__(type.name)
    self.type = type
    self.isa = isa
    self.imports = imports
    self.typedecls = typedecls
    self.methods = methods
    self._fillscope()
  def _fillscope(self):
    self.scope = scope.Scope(self)
    if isinstance(self.type, GenericTypename):
      for a in self.type.args:
        self.scope.define(a)
    self.scope.define(self.type, name='This')
    for a in self.isa + self.imports + self.typedecls:
      self.scope.define(a)
    for a in self.methods:
      self.scope.define(a, name=a.name)

    ctx().acquire_pending(self.scope)

class ChoiceDecl(_NameEq, CGlobalName, TypeDef):
  def __init__(self, choice, typearg=None):
    super(ChoiceDecl, self).__init__(choice)
    self.value = None
    self.typearg = typearg
    self.typedecl = None
    self.codeloc = CodeLoc(self)
  def _defbuiltins(self):
    if self.typearg is not None:
      args = [VarDecl('arg', self.typearg)]
    else:
      args = []
    init = Initializer(self.typedecl.type,
        [('which', ExprLiteral(self.value))])
    self.ctor = FunctionDecl('Ctor', [], args, [self.typedecl.type], Block([Return(init)]))
    self.valuevar = VarDecl('Value', Type('U32'), ExprLiteral(self.value))
    self._fillscope()
  def _fillscope(self):
    self.scope = scope.Scope(self)
    self.scope.define(self.ctor)
    self.scope.define(self.valuevar)
  def typecheck(self):
    return self.typedecl.type

class TypeDecl(_NameEq, CGlobalName, HasDep, Decl, TypeDef):
  REC, TAGGEDUNION, ENUM, UNION, FORWARD = range(5)
  def __init__(self, type, isa, imports, typedecls, decls, methods, funs):
    super(TypeDecl, self).__init__(type.name)
    self.type = type
    self.isa = isa
    self.imports = imports
    self.typedecls = typedecls

    self.kind = self.whatkind(decls)
    self.decls = decls

    i = 0
    for d in decls:
      if isinstance(d, ChoiceDecl):
        d.typedecl = self
        d.value = i
        d._defbuiltins()
        i += 1
    if self.kind == TypeDecl.TAGGEDUNION:
      union = Union(Type('__as'), [UnionField(d.name, d.typearg) for d in decls])
      self.typedecls.append(union)
      self.decls.append(FieldDecl('__unsafe_as', Type('__as')))
    if self.kind == TypeDecl.TAGGEDUNION or self.kind == TypeDecl.ENUM:
      self.decls.append(FieldDecl('which', Type('U32')))

    self.methods = methods
    self.funs = funs
    self._fillscope()

  def _fillscope(self):
    self.scope = scope.Scope(self)
    if isinstance(self.type, GenericTypename):
      for a in self.type.args:
        self.scope.define(a)
    self.scope.define(self.type, name='This')
    for a in self.isa + self.imports + self.typedecls:
      self.scope.define(a)
    for a in self.decls:
      self.scope.define(a, name=a.name)
    for a in self.methods:
      self.scope.define(a, name=a.name)
      a.scope.define(VarDecl('this', TypeRef(a.access, self.type)))
    for a in self.funs:
      self.scope.define(a, name=a.name)

    ctx().acquire_pending(self.scope)

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

  def dependson(self):
    if isinstance(self.type, GenericTypename):
      return [self.type]
    else:
      return []

  def typecheck(self):
    return self

class TupleDecl(_NameEq, HasDep):
  def __init__(self, type):
    super(TupleDecl, self).__init__(type.name)
    self.type = type
  def dependson(self):
    return [self.type.types]

class TypeAppDecl(_NameEq, HasDep, Decl):
  def __init__(self, typeapp):
    super(TypeAppDecl, self).__init__(typeapp.name)
    self.typeapp = typeapp
    self.typedecl = None
    self.inscope = None
  def dependson(self):
    return [typeapp.type] + typeapp.args

class UnionField(_NameEq, CGlobalName):
  def __init__(self, name, type):
    super(UnionField, self).__init__(name)
    self.type = type

class Union(_NameEq, CGlobalName, Decl, TypeDef):
  def __init__(self, type, fields):
    super(Union, self).__init__(type.name)
    self.type = type
    self.fields = fields
    self._fillscope()
  def _fillscope(self):
    self.scope = scope.Scope(self)
    for f in self.fields:
      if f is not None:
        self.scope.define(f)
  def dependson(self):
    return [t for _,t in self.pairs]

class FunctionDecl(_NameEq, CGlobalName, Decl, TypeDef):
  def __init__(self, name, genargs, args, returns, body):
    super(FunctionDecl, self).__init__(name)
    self.genargs = genargs
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
    if self.body is not None:
      self.scope.define(self.body)
    for a in self.genargs:
      self.scope.define(a)

    ctx().acquire_pending(self.scope)

  def typecheck(self):
    return self
  def typedef(self):
    return self

class FunctionInstanceDecl(HasDep):
  def __init__(self, call):
    self.call = call
    self.fundecl = None
    self.inscope = None
  def dependson(self):
    return [t.typecheck() for t in self.call.terms]

class MethodDecl(FunctionDecl):
  def __init__(self, name, genargs, access, args, returns, body):
    self.access = access
    super(MethodDecl, self).__init__(name, genargs, args, returns, body)

class VarDecl(_NameEq, Decl):
  def __init__(self, name, type, expr=None):
    super(VarDecl, self).__init__(name)
    self.type = type
    self.expr = expr
    self.mutatingblock = None
    self.optionalarg = False
    self.scope = scope.Scope(self)
  def typecheck(self):
    if self.type is not None:
      if self.expr is not None:
        typing.checkcompat(self.type, self.expr.typecheck())
      return self.type
    else:
      return self.expr.typecheck()
  def typedef(self):
    return self.typecheck()
  def setmutatingblock(self, b):
    self.mutatingblock = b
    if self.mutatingblock is not None:
      self.scope.define(self.mutatingblock)

class FieldConstDecl(_NameEq, Decl, CGlobalName):
  def __init__(self, vardecl):
    super(FieldConstDecl, self).__init__(vardecl.name)
    self.vardecl = vardecl

class FieldDecl(VarDecl):
  def __init__(self, name, type):
    super(FieldDecl, self).__init__(name, type)
    delattr(self, 'scope')

class Expr(_FieldsEq):
  def __init__(self):
    super(Expr, self).__init__()
    self.maybeuncall = False
  def typecheck(self):
    raise Exception("Not implemented for type '%s'" % type(self))
  def typedef(self):
    return self.typecheck().typedef()

class Value(Expr):
  def __init__(self, name):
    super(Value, self).__init__()
    self.name = name
  def typecheck(self):
    return scope.current().q(self).typecheck()

class Ref(Expr):
  def __init__(self, access, value):
    super(Ref, self).__init__()
    if access == '&!':
      self.access = '!'
    else:
      self.access = '.'
    self.value = value
  def typecheck(self):
    return TypeRef(self.access, self.value.typecheck())

class Deref(Expr):
  def __init__(self, access, value):
    super(Deref, self).__init__()
    self.access = access
    self.value = value
  def typecheck(self):
    return self.value.typecheck().deref(self.access)

class ValueField(Expr):
  def __init__(self, container, access, field):
    super(ValueField, self).__init__()
    self.container = container
    self.access = access
    self.field = field
  def typecheck(self):
    return scope.current().q(self).typecheck()

class ExprLiteral(Expr):
  def __init__(self, lit):
    super(ExprLiteral, self).__init__()
    self.terms = [lit]
  def typecheck(self):
    if isinstance(self.terms[0], basestring):
      return Type('nlang.literal.String')
    elif isinstance(self.terms[0], bool):
      return Type('nlang.literal.Bool')
    else:
      return Type('nlang.literal.Integer')

class ExprNull(Expr):
  def __init__(self):
    super(ExprNull, self).__init__()
  def typecheck(self):
    return Type('nlang.literal.Null')

class ExprSizeof(Expr):
  def __init__(self):
    super(ExprSizeof, self).__init__()
  def typecheck(self):
    return Type('Size')

class ExprIdent(Expr):
  def __init__(self, ident):
    super(ExprIdent, self).__init__()
    self.name = ident
    self.terms = []
  def typecheck(self):
    return scope.current().q(self).type

class Tuple(Expr):
  def __init__(self, *args):
    super(Tuple, self).__init__()
    self.terms = list(args)
  def typecheck(self):
    return TypeTuple(*[t.typecheck() for t in self.terms])

class ConstrainedExpr(Expr):
  def __init__(self, expr, type):
    super(ConstrainedExpr, self).__init__()
    self.terms = [expr]
    self.type = type
  def typecheck(self):
    return typing.unify([self.typecheck(), self.terms[0].typecheck()])

class BinExpr(Expr):
  def __init__(self, op, left, right):
    super(BinExpr, self).__init__()
    self.op = op
    self.terms = [left, right]
  def typecheck(self):
    return typing.unify([t.typecheck() for t in self.terms])

class CmpBinExpr(BinExpr):
  def typecheck(self):
    typing.unify([t.typecheck() for t in self.terms])
    return Type('Bool')

class BoolBinExpr(BinExpr):
  def __init__(self, op, left, right):
    super(CmpBinExpr, self).__init__()
    self.op = op
    self.terms = [left, right]
  def typecheck(self):
    return typing.unify([t.typecheck() for t in self.terms] + [Type('Bool')])

class IsaExpr(BinExpr):
  def __init__(self, op, left, right):
    super(CmpBinExpr, self).__init__()
    self.op = op
    self.terms = [left, right]
  def typecheck(self):
    return Type('Bool')

class UnExpr(Expr):
  def __init__(self, op, expr):
    super(UnExpr, self).__init__()
    self.op = op
    self.terms = [expr]
  def typecheck(self):
    return typing.unify([t.typecheck() for t in self.terms])

class Call(Expr):
  def __init__(self, fun, args):
    super(Call, self).__init__()
    self.terms = [fun] + args

    # The function may or may not be instantiating a generic.
    # But we don't know that yet.
    self.funinstancedecl = FunctionInstanceDecl(self)
    ctx().funinstances_pending.append(self.funinstancedecl)

  def typecheck(self):
    fun = self.terms[0]
    if isinstance(fun, ExprSizeof):
      assert len(self.terms) == 2
      return Type('Size')

    fun = scope.current().q(fun)
    d = fun.typedef()
    if isinstance(d, FunctionDecl):
      for i in xrange(1, len(self.terms)):
        typing.unify([d.args[i-1].type, self.terms[i].typecheck()])
      return d.rettype
    elif isinstance(d, ChoiceDecl):
      return d.typedecl.type
    else:
      raise errors.TypeError("'%s' is not a function, at %s" % (fun, self.codeloc))

class UnCall(Expr):
  def __init__(self, fun):
    super(UnCall, self).__init__()
    self.terms = [fun]

class DefaultCtor(Call):
  def __init__(self, type):
    # FIXME: Should call ctor if in the same module or a submodule.
    super(DefaultCtor, self).__init__(ValueField(type, '.', 'Ctor'), [])

class Dtor(Call):
  def __init__(self, type):
    super(Dtor, self).__init__(ValueField(type, '.', 'Dtor'), [])

class Initializer(Expr):
  def __init__(self, value, pairs):
    super(Initializer, self).__init__()
    self.value = value
    self.pairs = pairs
  def typecheck(self):
    return self.value.typecheck()

class Assign(_FieldsEq):
  def __init__(self, value, expr):
    super(Assign, self).__init__()
    self.value = value
    self.expr = expr
  def typecheck(self):
    typing.checkcompat(self.value.typecheck(), self.expr.typecheck())
  def typedef(self):
    return self.typecheck()

class Return(_FieldsEq):
  def __init__(self, expr):
    super(Return, self).__init__()
    self.expr = expr
  def typedef(self):
    return self.typecheck()

class Continue(_FieldsEq):
  pass

class Break(_FieldsEq):
  pass

class Assert(_FieldsEq):
  def __init__(self, expr):
    super(Assert, self).__init__()
    self.expr = expr

class While(_FieldsEq, Decl):
  def __init__(self, cond, body):
    super(While, self).__init__()
    self.cond = cond
    self.body = body
    self._fillscope()
  def _fillscope(self):
    self.scope = scope.Scope(self)
    if self.body is not None:
      self.scope.define(self.body)

class For(_FieldsEq, Decl):
  def __init__(self, vardecl, iter, body):
    super(For, self).__init__()
    self.vardecl = vardecl
    self.iter = iter
    self.body = body
    self._fillscope()
  def _fillscope(self):
    self.scope = scope.Scope(self)
    self.scope.define(self.vardecl)
    if self.body is not None:
      self.scope.define(self.body)

class PFor(_FieldsEq, Decl):
  def __init__(self, vardecl, iter, body):
    super(PFor, self).__init__()
    self.vardecl = vardecl
    self.iter = iter
    self.body = body
    self._fillscope()
  def _fillscope(self):
    self.scope = scope.Scope(self)
    self.scope.define(self.vardecl)
    if self.body is not None:
      self.scope.define(self.body)

class If(_FieldsEq, Decl):
  def __init__(self, condpairs, elsebody):
    super(If, self).__init__()
    self.condpairs = condpairs
    self.elsebody = elsebody
    self._fillscope()
  def _fillscope(self):
    self.scope = scope.Scope(self)
    for _, b in self.condpairs:
      self.scope.define(b)
    if self.elsebody is not None:
      self.scope.define(self.elsebody)
  def typecheck(self):
    bodies = [b for _,b in self.condpairs]
    if self.elsebody is not None:
      bodies.append(self.elsebody)
    return typing.unify(bodies)
  def typedef(self):
    return self.typecheck()

class Block(_FieldsEq, Decl):
  def __init__(self, body):
    super(Block, self).__init__()
    self.body = body
    self._fillscope()
  def _fillscope(self):
    self.scope = scope.Scope(self)
    for d in self.body:
      if isinstance(d, Decl):
        self.scope.define(d)
  def typecheck(self):
    if len(self.body) == 0:
      return Type('Void')
    else:
      return self.body[-1].typecheck()
  def typedef(self):
    return self.typecheck()

class Future(Block):
  pass

class Matcher(_FieldsEq):
  def __init__(self, pattern, body):
    self.pattern = pattern
    self.body = body
    self.expr = None

class Match(_FieldsEq):
  def __init__(self, expr, matchers):
    self.expr = expr
    self.matchers = matchers
    for m in self.matchers:
      m.expr = self.expr

class SemanticAssert(_FieldsEq):
  def __init__(self, expr):
    super(SemanticAssert, self).__init__()
    self.expr = expr

class SemanticClaim(_FieldsEq):
  def __init__(self, expr):
    super(SemanticClaim, self).__init__()
    self.expr = expr

class Import(_NameEq):
  def __init__(self, path, all=False, alias=None):
    self.modname = '.'.join(path[:-1])
    super(Import, self).__init__('.'.join(path))
    self.path = path
    self.all = all
    self.alias = alias

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
    self.typeapps = []
    self.typeapps_pending = []
    self.funinstances = []
    self.funinstances_pending = []
    self.tuples = []
    self.tuples_pending = []

  def acquire_pending(self, scope):
    '''scope.container must be toplevel'''

    scope.gendecls.extend(self.typeapps_pending)
    #for v in self.typeapps_pending:
    #  self.typeapps.append((scope, v))
    self.typeapps_pending = []

    scope.gendecls.extend(self.tuples_pending)
    self.tuples_pending = []

    scope.gendecls.extend(self.funinstances_pending)
    #for v in self.funinstances_pending:
    #  self.funinstances.append((scope, v))
    self.funinstances_pending = []


class PlaceholderModule(_NameEq):
  '''When importing module 'a.b.c', modules 'a' and 'a.b' are not imported.
  However, we need something to return when looking up 'a', or 'a.b' in the
  current scope (in which 'a.b.c' has been imported). A PlaceholderModule
  is returned. If, later, 'a' is imported, then the PlaceholderModule is
  replaced with the actual Module 'a' and the scope of the PlaceholderModule
  (that contains a reference to 'a.b') is copied over.
  '''
  def __init__(self, name):
    super(PlaceholderModule, self).__init__(name)
    self.scope = scope.Scope(self)

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
    for x in self.toplevels:
      self.scope.define(x)
