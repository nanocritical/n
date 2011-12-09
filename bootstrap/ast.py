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
    raise TypeError("Cannot dereference type '%s'" % self.name)
  def typecheck(self):
    return self
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
      raise TypeError("Cannot mutate the type '%s'" % self)
    return self.type
  def deconstruct(self, genscope, t):
    _checkdeconstructcompat(self, t)
    if self.access != t.access:
      # We may actually want to be tolerant here: does '.' matche '!' ?
      raise errors.PmStructError(self, t)
    return self.type.deconstruct(genscope, t.type)

class TypeRefNullable(TypeRef):
  def __init__(self, typeref):
    super(TypeRefNullable, self).__init__(typeref.access, typeref.type)

class TypeTuple(Type):
  def __init__(self, *types):
    super(TypeTuple, self).__init__(', '.join([t.name for t in types]))
    self.types = types

    modtuples = ctx().tuples_pending
    if self.type not in modtuples:
      modtuples['nlang.Tuple'] = {}
    if self not in modtuples['nalng.Tuple']:
      modtuples['nlang.Tuple'][self] = TupleDecl(self)
    self.tupledecl = modtuples['nlang.Tuple'][self]

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

    modtypeapps = ctx().typeapps_pending
    if self.type not in modtypeapps:
      modtypeapps[self.type] = {}
    if self not in modtypeapps[self.type]:
      modtypeapps[self.type][self] = TypeAppDecl(self)
    self.typeappdecl = modtypeapps[self.type][self]

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

class Intf(_NameEq, Decl):
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

class DynIntf(_NameEq, Decl):
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

class ChoiceDecl(_NameEq, CGlobalName):
  def __init__(self, choice, typearg=None):
    if isinstance(choice, Assign):
      super(ChoiceDecl, self).__init__(choice.value)
      self.value = choice.expr
    else:
      self.name = choice
      self.value = None
    self.typearg = typearg

class TypeDecl(_NameEq, CGlobalName, HasDep, Decl):
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
      a.scope.define(VarDecl('this', TypeRef(a.access, self)))
    for a in self.funs:
      self.scope.define(a, name=a.name)

    ctx().acquire_pending(self.scope)

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

  def dependson(self):
    if isinstance(self.type, GenericTypename):
      return [self.type]
    else:
      return []

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

class Union(TypeDecl):
  pass

class FunctionDecl(_NameEq, CGlobalName, Decl):
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
    for a in self.body:
      if isinstance(a, Decl):
        self.scope.define(a)
    for a in self.genargs:
      self.scope.define(a)

    ctx().acquire_pending(self.scope)

  def typecheck(self):
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
      return self.type
    else:
      return self.expr.typecheck()
  def setmutatingblock(self, b):
    self.mutatingblock = b
    for d in self.mutatingblock:
      if isinstance(d, Decl):
        self.scope.define(d)

class FieldDecl(VarDecl):
  def __init__(self, name, type):
    super(FieldDecl, self).__init__(name, type)
    delattr(self, 'scope')

class Expr(_FieldsEq):
  def __init__(self):
    super(Expr, self).__init__()
  def typecheck(self):
    raise Exception("Not implemented for type '%s'" % type(self))

class Value(Expr):
  def __init__(self, name):
    super(Value, self).__init__()
    self.name = name
  def typecheck(self):
    return scope.current().q(self).type

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
    self.name = container
    self.access = access
    self.field = field
  def typecheck(self):
    return scope.current().q(self).type

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
    modfuninstances = ctx().funinstances_pending
    if fun.name not in modfuninstances:
      modfuninstances[fun.name] = []  # Do not use a dict, no good way to hash self.
    modfuninstances[fun.name].append(FunctionInstanceDecl(self))
    self.funinstancedecl = modfuninstances[fun.name][-1]

  def typecheck(self):
    fun = self.terms[0]
    if isinstance(fun, ExprSizeof):
      assert len(self.terms) == 2
      return Type('Size')

    fun = scope.current().q(fun)
    if not isinstance(fun, FunctionDecl):
      raise TypeError("'%s' is not a function" % funname)

    for i in xrange(1, len(self.terms)):
      typing.unify([fun.args[i-1], self.terms[i]])

    return fun.rettype

class Initializer(Expr):
  def __init__(self, type, pairs):
    super(Initializer, self).__init__()
    self.type = type
    self.pairs = pairs
  def typecheck(self):
    return self.type

class Assign(_FieldsEq):
  def __init__(self, value, expr):
    super(Assign, self).__init__()
    self.value = value
    self.expr = expr
  def typecheck(self):
    typing.checkcompat(self.value.typecheck(), self.expr.typecheck())

class Return(_FieldsEq):
  def __init__(self, expr):
    super(Return, self).__init__()
    self.expr = expr

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
    for d in self.body:
      if isinstance(d, Decl):
        self.scope.define(d)

    ctx().acquire_pending(self.scope)

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
    for d in self.body:
      if isinstance(d, Decl):
        self.scope.define(d)

    ctx().acquire_pending(self.scope)

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
    for d in self.body:
      if isinstance(d, Decl):
        self.scope.define(d)

    ctx().acquire_pending(self.scope)

class If(_FieldsEq, Decl):
  def __init__(self, condpairs, elsebody):
    super(If, self).__init__()
    self.condpairs = condpairs
    self.elsebody = elsebody
    self._fillscope()
  def _fillscope(self):
    self.scope = scope.Scope(self)
    for _, b in self.condpairs:
      for d in b:
        if isinstance(d, Decl):
          self.scope.define(d)
    if self.elsebody is not None:
      for d in self.elsebody:
        if isinstance(d, Decl):
          sself.scope.define(d)

    ctx().acquire_pending(self.scope)

class Block(_FieldsEq, Decl):
  def __init__(self, block):
    super(Block, self).__init__()
    self.block = block
    self._fillscope()
  def _fillscope(self):
    self.scope = scope.Scope(self)
    for d in self.block:
      if isinstance(d, Decl):
        self.scope.define(d)

    ctx().acquire_pending(self.scope)

class Future(_FieldsEq, Decl):
  def __init__(self, block):
    super(Future, self).__init__()
    self.block = block
    self._fillscope()
  def _fillscope(self):
    self.scope = scope.Scope(self)
    for d in self.block:
      if isinstance(d, Decl):
        self.scope.define(d)

    ctx().acquire_pending(self.scope)

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
    self.typeapps = {}
    self.typeapps_pending = {}
    self.funinstances = {}
    self.funinstances_pending = {}
    self.tuples = {}
    self.tuples_pending = {}

    self.gendecls = []

  def acquire_pending(self, scope):
    for k,v in self.typeapps_pending.iteritems():
      scope.gendecls.extend(v.itervalues())
      self.typeapps[k] = v
    self.typeapps_pending.clear()
    for k,v in self.tuples_pending.iteritems():
      scope.gendecls.extend(v.itervalues())
      self.tuples[k] = v
    self.tuples_pending.clear()
    for k,v in self.funinstances_pending.iteritems():
      scope.gendecls.extend(v)
      self.funinstances[k] = v
    self.funinstances_pending.clear()

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
