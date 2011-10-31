import re

import errors
import scope
import parser

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


class Type(_NameEq):
  def __init__(self, name):
    super(Type, self).__init__(Type.normalize(name))

  @staticmethod
  def normalize(name):
    return re.sub(r'\s*,\s+', ', ', re.sub(r'\s+', ' ', name))

  @classmethod
  def parse(cls, args):
    return cls(args)

class TypeRef(Type):
  def __init__(self, access, type):
    super(TypeRef, self).__init__(access + type.name)
    self.access = access
    self.type = type

  @classmethod
  def parse(cls, args):
    return cls(args[0], args[1])

class TypeTuple(Type):
  def __init__(self, *types):
    super(TypeTuple, self).__init__(', '.join([t.name for t in types]))
    self.types = types
    gmodctx.tuples.add(TupleDecl(self))

  @classmethod
  def parse(cls, args):
    return cls(*args)

class TypeApp(Type):
  def __init__(self, type, *args):
    super(TypeApp, self).__init__(' '.join([t.name for t in [type] + list(args)]))
    self.type = type
    self.args = args
    if self.type.name not in gctx[gmodname].typeapps:
      gctx[gmodname].typeapps[self.type.name] = set()
    gctx[gmodname].typeapps[self.type.name].add(TypeAppDecl(self))

  @classmethod
  def parse(cls, args):
    return cls(args[0], *args[1:])

class TypeGeneric(Type):
  def __init__(self, type, *args):
    super(TypeGeneric, self).__init__(type.name)
    self.type = type
    self.args = args

  @classmethod
  def parse(cls, args):
    return cls(args[0], *args[1:])

class TypeSlice(TypeApp):
  def __init__(self, access, type):
    super(TypeSlice, self).__init__(Type('Slice'), TypeRef(access, type))

  @classmethod
  def parse(cls, args):
    return cls(args[1], args[2])

class ChoiceDecl(_NameEq):
  def __init__(self, choice, typearg=None):
    if isinstance(choice, Assign):
      self.name = choice.var
      self.value = choice.expr
    else:
      self.name = choice
      self.value = None
    self.typearg = typearg

  @classmethod
  def parse(cls, args):
    if len(args) == 2:
      return cls(ChoiceDecl(args[1]))
    else:
      return cls(ChoiceDecl(args[1], args[3]))
    return cls(args)


class TypeDecl(_NameEq):
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
    for a in self.decls + self.methods:
      self.scope.define(a, name=a.name)
    for a in self.funs:
      self.scope.define(a, name=a.name)

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

  @classmethod
  def parse(cls, args):
    assert args[0] == 'type'
    assert args[2] == '='
    type = args[1]
    isa = []
    imports = cls.extract(Import, args)
    typedecls = cls.extract(TypeDecl, args)
    decls = cls.extract(ChoiceDecl, args) + cls.extract(FieldDecl, args)
    methods = cls.extract(Method, args)
    funs = cls.extract(Function, args)
    return cls(type, isa, imports, typedecls, decls, methods, funs)

class TupleDecl(TypeDecl):
  def __init__(self, type):
    super(TypeDecl, self).__init__(type.name)
    self.type = type

class TypeAppDecl(TypeDecl):
  def __init__(self, type):
    super(TypeDecl, self).__init__(type.name)
    self.type = type

class Union(TypeDecl):
  pass

class Function(_NameEq):
  def __init__(self, name, args, returns, body):
    self.name = name
    self.args = args
    self.returns = returns
    self.body = body
    if len(self.returns) > 1:
      self.rettype = TypeTuple(self.returns)
    else:
      self.rettype = self.returns[0]
    self._fillscope()

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

  @classmethod
  def parse(cls, args):
    return cls.doparse(args)

class Method(Function):
  def __init__(self, name, args, returns, body):
    super(Method, self).__init__(name, args, returns, body)
  @classmethod
  def parse(cls, args):
    access = '.'
    if args[1] == '!':
      access = '!'
      args = args[0:1] + args[2:]
    r = cls.doparse(args)
    r.access = access
    return r

class VarDecl(_NameEq):
  def __init__(self, name, type, expr=None):
    super(VarDecl, self).__init__(name)
    self.type = type
    self.expr = expr
    self.scope = scope.Scope(self)

  @classmethod
  def parse(cls, args):
    if args[0] == 'let':
      if isinstance(args[2], list):
        args = args[1:2] + args[2]
      else:
        args = args[1:]

    if isinstance(args[0], list):
      return [cls.parse([a] + args[1:]) for a in args[0]]
    else:
      v = None
      if isinstance(args[0], VarDecl):
        v = args[0]
        v.expr = args[2]
      elif args[1] == ':':
        v = cls(args[0], args[2])
      elif args[1] == '=':
        if isinstance(args[0], basestring):
          v = cls(args[0], None, args[2])
        else:
          v = args[0]
          v.expr = args[2]
      else:
        raise Exception()
      return v

class FieldDecl(VarDecl):
  pass

class Assign(_FieldsEq):
  def __init__(self, var, expr):
    self.var = var
    self.expr = expr

  @classmethod
  def parse(cls, args):
    return cls(args[0], args[2])

class Expr(_FieldsEq):
  def __init__(self, term):
    self.term = term

  @classmethod
  def parse(cls, args):
    return cls(args)

class Literal(Expr):
  pass

class Ident(_NameEq):
  def __init__(self, name):
    super(Ident, self).__init__(name)

  @classmethod
  def parse(cls, args):
    return cls(args)

class Tuple(_FieldsEq):
  def __init__(self, *args):
    self.terms = list(args)

  @classmethod
  def parse(cls, args):
    assert len(args) % 2 == 1
    r = []
    for t in args:
      if isinstance(t, basestring) and t == ',':
        continue
      r.append(t)
    return cls(*r)

class ConstrainedExpr(_FieldsEq):
  def __init__(self, expr, type):
    self.expr = expr
    self.type = type

  @classmethod
  def parse(cls, args):
    assert len(args) == 3
    assert args[1] == ':'
    return cls(args[0], args[2])

class BinOp(_FieldsEq):
  def __init__(self, op, left, right):
    self.op = op
    self.left = left
    self.right = right

  @classmethod
  def parse(cls, args):
    assert len(args) == 3
    return cls(args[1], args[0], args[2])

class UnOp(_FieldsEq):
  def __init__(self, op, expr):
    self.op = op
    self.expr = expr

  @classmethod
  def parse(cls, args):
    assert len(args) == 2
    return cls(args[0], args[1])

class Call(_FieldsEq):
  def __init__(self, fun, args):
    self.fun = fun
    self.args = args

  @classmethod
  def parse(cls, args):
    return cls(args[0], _listwrap(args[1]))

class Return(_FieldsEq):
  def __init__(self, expr):
    self.expr = expr
    self.type = None

  @classmethod
  def parse(cls, args):
    return cls(args[1])

class Assert(_FieldsEq):
  def __init__(self, expr):
    self.expr = expr

  @classmethod
  def parse(cls, args):
    return cls(args[1])

class SemanticAssert(_FieldsEq):
  def __init__(self, expr):
    self.expr = expr

  @classmethod
  def parse(cls, args):
    return cls(args[1])

class SemanticClaim(_FieldsEq):
  def __init__(self, expr):
    self.expr = expr

  @classmethod
  def parse(cls, args):
    return cls(args[1])

class Import(_NameEq):
  def __init__(self, name, modname):
    super(Import, self).__init__(name)
    self.module = parser.parsemod(modname)

  @classmethod
  def parse(cls, args):
    args = args[:-1]  # Remove tailing '\n'
    if args[0] == 'import':
      return [cls(a, a) for a in args[1:]]
    elif args[0] == 'from':
      mod = args[1]
      return [cls(a, mod) for a in args[3:]]

gctx = {}
gmodctx = None
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

  @classmethod
  def parse(cls, args):
    return cls(args)
