import parser
import errors
import resolv
import copy
import typing
import ast
from ast import *
from scope import scopedname, down, up

def _p(out, *args):
  assert len(args) > 0
  for a in args:
    if isinstance(a, basestring):
      out.write(a)
    elif isinstance(a, int) or isinstance(a, long):
      out.write(str(a))
    elif isinstance(a, Expr) and a.maybeuncall:
      UnCall(a).cwrite(out)
    else:
      a.cwrite(out)

gnextsym = 0

def _gensym():
  global gnextsym
  n = gnextsym
  gnextsym += 1
  return '__nlang_gensym_' + str(n) + '__'

grettype = []

def w(self, out):
  global depth
  _p(out, scopedname(self))
Type.cwrite = w

depth = 0
def w(self, out):
  global depth
  if depth == 1:
    raise None
  depth += 1
  if self.access == '.':
    _p(out, 'nlangcp__')
  else:
    _p(out, 'nlangp__')
  _p(out, self.type)
  depth -= 1
TypeRef.cwrite = w

def w(self, out):
  _p(out, 'nlangtuple__' + '_'.join([scopedname(t) for t in self.types]))
TypeTuple.cwrite = w

def w(self, out):
  _p(out, 'nlangapp__', self.type, '_')
  for i in xrange(len(self.args)):
    _p(out, self.args[i])
    if i < len(self.args) - 1:
      _p(out, '_')
TypeApp.cwrite = w

def w(self, out):
  _p(out, 'nlangapp__', self.type, '_')
  for i in xrange(len(self.args)):
    _p(out, self.args[i].instantiated)
    if i < len(self.args) - 1:
      _p(out, '_')
GenericTypename.cwrite = w

def w(self, out):
  pass
Intf.cwrite = w
DynIntf.cwrite = w

def w(self, out):
  _p(out, scopedname(self.typedecl) + '_' + self.name)
ChoiceDecl.cwrite = w

def w(self, out):
  if isinstance(self.type, GenericTypename) \
      and self.type.args[0].instantiated is None:
    if self in ast.ctx().typeapps:
      for tappd in ast.ctx().typeapps[self].itervalues():
        tappd.typedecl = self
    return

  if self.kind == TypeDecl.FORWARD:
    _p(out, 'struct ', self.type, ';\n')
    return

  down(self.scope)
  if self.kind == TypeDecl.TAGGEDUNION or self.kind == TypeDecl.ENUM:
    _p(out, indent(+1), 'typedef enum {\n')
    for d in self.decls:
      if isinstance(d, ChoiceDecl):
        _p(out, indent(), d, ',\n')
    indent(-1)
    _p(out, indent(), '} nlangtag__', self.type, ';\n\n')

  for gen in self.scope.gendecls:
    _p(out, gen)

  for td in self.typedecls:
    _p(out, td)

  _p(out, indent(+1), 'typedef struct {\n')
  for d in self.decls:
    if not isinstance(d, ChoiceDecl):
      _p(out, indent(), d, ';\n')
  indent(-1)
  _p(out, indent(), '}', self.type, ';\n')
  _p(out, indent(), 'typedef ', self.type, '* nlangp__', self.type, ';\n')
  _p(out, indent(), 'typedef const ', self.type, '* nlangcp__', self.type, ';\n\n')

  for d in self.decls:
    if isinstance(d, ChoiceDecl):
      down(d.scope)
      _p(out, d.ctor)
      _p(out, FieldConstDecl(d.valuevar), ';\n')
      up()

  for d in self.methods:
    _p(out, d)
  for d in self.funs:
    _p(out, d)
  up()
TypeDecl.cwrite = w

def w(self, out):
  _p(out, 'typedef struct {\n')
  for i in xrange(len(self.type.types)):
    _p(out, scopedname(self.type.types[i]), ' t', i, ';\n')
  _p(out, '}', self.type, ';\n\n')
TupleDecl.cwrite = w

def w(self, out):
  self.typedecl = scope.current().q(self.typeapp)
  applied = copy.deepcopy(self.typedecl)
  for genarg, instancearg in zip(applied.type.args, self.typeapp.args):
    genarg.instantiated = instancearg
  _p(out, applied)
TypeAppDecl.cwrite = w

def w(self, out):
  _p(out, 'typedef union {\n')
  for f in self.fields:
    if f is not None:
      if f.type is not None:
        _p(out, '  ', f.type, ' ', f.name, ';\n')
  _p(out, '} ', self.type, ';\n\n')
  _p(out, 'typedef ', self.type, '* nlangp__', self.type, ';\n')
  _p(out, 'typedef const ', self.type, '* nlangcp__', self.type, ';\n\n')
Union.cwrite = w

def w(self, out):
  if len(self.genargs) > 0 and self.genargs[0].instantiated is None:
    if self.name in ast.ctx().funinstances:
      for gf in ast.ctx().funinstances[self.name]:
        gf.fundecl = self
    return

  down(self.scope)

  for gen in self.scope.gendecls:
    _p(out, gen)

  global grettype
  grettype.append(self.rettype)
  if self.name[0].islower() and self.name != 'main':
    _p(out, 'static ')
  _p(out, self.rettype, '\n', scopedname(self), '(')
  if isinstance(self, MethodDecl):
    objtype = self.scope.parent.container.type
    _p(out, TypeRef(self.access, objtype), ' this')
    if len(self.args) > 0:
      _p(out, ', ')
  for i in xrange(len(self.args)):
    _p(out, self.args[i])
    if i != len(self.args) - 1:
      _p(out, ', ')
  if self.body is None:
    _p(out, ');\n')
  else:
    _p(out, ') ', self.body, '\n\n')
  up()
  grettype.pop()
MethodDecl.cwrite = w
FunctionDecl.cwrite = w

def w(self, out):
  d = self.call.terms[0].typecheck().typedef()

  if isinstance(d, FunctionDecl):
    if len(d.genargs) == 0:
      # This call was not, in fact, to a generic.
      return

    instance = copy.deepcopy(d)
    for arg, argexpr in zip(instance.args, self.call.terms[1:]):
      arg.type.deconstruct(instance.scope, argexpr.typecheck())
    _p(out, instance)

  elif isinstance(d, TypeDecl):
    if not isinstance(d.type, GenericTypename):
      # This call was not, in fact, to a generic.
      return

    applied = copy.deepcopy(d)
    for genarg, instancearg in zip(applied.type.args, self.call.terms[0]):
      genarg.instantiated = instancearg
    _p(out, DefaultCtor(applied))

  else:
    raise Exception()
FunctionInstanceDecl.cwrite = w

def w(self, out):
  if self.mutatingblock is not None:
    _p(out, 'const ', self.typecheck(), ' ', scopedname(self), ' = ({ ')
    _p(out, self.typecheck(), ' ', scopedname(self))
    if self.expr is not None:
      _p(out, ' = ', self.expr)
    _p(out, ';\n', self.mutatingblock)
    _p(out, scopedname(self), '; })')
  else:
    _p(out, self.typecheck(), ' ', scopedname(self))
    if self.expr is not None:
      _p(out, ' = ', self.expr)
VarDecl.cwrite = w

def w(self, out):
  _p(out, self.vardecl.typecheck(), ' ', scopedname(self))

  if self.vardecl.expr is not None:
    _p(out, ' = ', self.vardecl.expr)
  if self.vardecl.mutatingblock is not None:
    raise Exception("Unsupported")
FieldConstDecl.cwrite = w

def w(self, out):
  tmp = _gensym()
  _p(out, '({ ', self.value, ' ', tmp, ';\n')
  indent(+2)
  _p(out, indent(), 'memset(&', tmp, ', 0, sizeof(', tmp, '));\n')

  typedecl = scope.current().q(self.value)
  for field, expr in self.pairs:
    found = False
    for f in typedecl.decls:
      if isinstance(f, FieldDecl) and f.name == field:
        _p(out, indent(), tmp, '.', field, ' = ', expr, ';\n')
        found = True
        break
    if not found:
      raise errors.ParseError("In initializer for type '%s', invalid field '%s', at %s" \
          % (self.value, field, self.value.codeloc))

  _p(out, indent(-2), tmp, '; })')
Initializer.cwrite = w

def w(self, out):
  self.typecheck()
  _p(out, self.value, '=', self.expr)
Assign.cwrite = w

def w(self, out):
  _p(out, scopedname(self))
Value.cwrite = w

def w(self, out):
  _p(out, '&', self.value)
Ref.cwrite = w

def w(self, out):
  _p(out, '*', self.value)
Deref.cwrite = w

def w(self, out):
  ctype = self.container.typecheck()
  if isinstance(ctype, TypeRef):
    access = '->'
  else:
    access = '.'

  c = scope.current().q(self.container)
  if isinstance(c, ast.ChoiceDecl):
    sc = c.scope
    access = '_'
  else:
    sc = ctype.typedef().scope
    f = sc.table[self.field]
    if isinstance(f, ast.ChoiceDecl):
      access = '_'

  _p(out, self.container, access, self.field)
ValueField.cwrite = w

def w(self, out):
  expr = self.terms[0]
  if isinstance(expr, basestring):
    _p(out, '"', expr, '"')
  elif isinstance(expr, bool):
    if expr:
      _p(out, '1')
    else:
      _p(out, '0')
  else:
    _p(out, expr)
ExprLiteral.cwrite = w

def w(self, out):
  _p(out, 'null')
ExprNull.cwrite = w

def w(self, out):
  _p(out, '(', self.typecheck(), '){')
  for term in self.terms:
    _p(out, term, ', ')
  _p(out, '}')
Tuple.cwrite = w

def w(self, out):
  self.typecheck()
  _p(out, '((', self.type, ')(', self.terms[0], '))')
ConstrainedExpr.cwrite = w

def w(self, out):
  self.typecheck()
  _p(out, '(', self.terms[0], ' ', self.op, ' ', self.terms[1], ')')
BinExpr.cwrite = w

def w(self, out):
  self.typecheck()
  if self.op == 'neg':
    _p(out, '(- ', self.terms[0], ')')
  elif self.op == 'not':
    _p(out, '(! ', self.terms[0], ')')
  else:
    _p(out, '(', self.op, ' ', self.terms[0], ')')
UnExpr.cwrite = w

def w(self, out):
  if self.terms[0] == 'sizeof':
    assert len(self.terms) == 2
    _p(out, 'sizeof(', self.terms[1], ')')
    return

  fun = scope.current().q(self.terms[0]).typedef()

  if isinstance(fun, ChoiceDecl):
    funexpr = ValueField(self.terms[0], '.', fun.ctor.name)
    fun = fun.ctor
  else:
    funexpr = self.terms[0]

  _p(out, funexpr, '(')
  if len(fun.args) == 0:
    _p(out, ')')
    return

  for i in xrange(len(fun.args)):
    if i + 1 < len(self.terms):
      _p(out, self.terms[i+1])
    else:
      if not fun.args[i].optionalarg:
        raise errors.ParseError("Non-optional argument '%s' is missing in call (%s), at %s" \
            % (fun.args[i], ' '.join(map(str, self.terms)), self.codeloc))
      _p(out, ExprNull())

    if i < len(fun.args) - 1:
      _p(out, ',')

  _p(out, ')')
Call.cwrite = w

def w(self, out):
  self.terms[0].maybeuncall = False
  fun = scope.current().q(self.terms[0]).typedef()

  if isinstance(fun, FunctionDecl) or isinstance(fun, ChoiceDecl):
    _p(out, Call(self.terms[0], []))
  else:
    _p(out, self.terms[0])
UnCall.cwrite = w

def w(self, out):
  global grettype
  typing.checkcompat(grettype[-1], self.expr.typecheck())
  _p(out, 'return (', self.expr, ')')
Return.cwrite = w

def w(self, out):
  _p(out, 'continue;\n')
Continue.cwrite = w

def w(self, out):
  _p(out, 'break;\n')
Break.cwrite = w

def w(self, out):
  _p(out, 'while (', self.cond, ')', self.body)
  _p(out, indent(), '}\n')
While.cwrite = w

def w(self, out):
  down(self.scope)
  range = _gensym()
  indent(+1)
  _p(out, '{\n', indent(), 'nlang_IndexRange ', range, ' = ', self.iter, ';\n')
  _p(out, indent(+1), 'for (; nlang_IndexRange_Next(&', range, ');) {\n')
  _p(out, indent(), self.vardecl, ' = nlang_IndexRange_Index(&', range, ');\n')
  _p(out, self.body)
  indent(-1)
  _p(out, '\n', indent(-1), '}\n', indent(), '}')
  up()
For.cwrite = w
PFor.cwrite = w

def w(self, out):
  _p(out, 'if (', self.condpairs[0][0], ')', self.condpairs[0][1])
  for cp in self.condpairs[1:]:
    _p(out, indent(), 'else if (', cp[0], ')', cp[1])

  if self.elsebody is not None:
    _p(out, indent(), 'else', self.elsebody)
If.cwrite = w

gblockdepth = 0
def indent(delta=0):
  global gblockdepth
  r = '  ' * gblockdepth
  gblockdepth += delta
  return r

def w(self, out):
  global gblockdepth
  down(self.scope)
  _p(out, indent(+1), '{\n')
  for b in self.body:
    _p(out, indent(), b, ';\n')
  indent(-1)
  _p(out, indent(), '}')
  up()
Block.cwrite = w

def w(self, out):
  exprtype = self.expr.typedef()
  if isinstance(exprtype, TypeDecl) \
      and (exprtype.kind == TypeDecl.TAGGEDUNION \
      or (exprtype.kind == TypeDecl.ENUM)):
    test = BinExpr('==', ValueField(self.expr, '.', 'which'),
        ValueField(ValueField(exprtype.type, '.', self.pattern.name), '.', 'Value'))
  else:
    test = BinExpr('==', self.expr, self.pattern)

  if self.first:
    _p(out, 'if (', test, ')', self.body)
  else:
    _p(out, indent(), 'else if (', test, ')', self.body)
Matcher.cwrite = w

def w(self, out):
  first = True
  for m in self.matchers:
    m.first = first
    _p(out, m)
    first = False
  _p(out, indent(), 'else { NLANG_UNREACHED(); }')
Match.cwrite = w

def w(self, out):
  _p(out, 'assert(', self.expr, ')')
Assert.cwrite = w

def w(self, out):
  pass
SemanticClaim.cwrite = w
SemanticAssert.cwrite = w

gimported = {}

def _importalias(out, path, alias):
  vpath = copy.copy(path)
  vpath[0] = Value(vpath[0])
  vardecl = VarDecl(alias, None,
      reduce((lambda a, b: ValueField(a, '.', b)), vpath))
  scope.current().define(vardecl)
  prefix = re.sub(r'\.', '_', scope.current().container.name)
  _p(out, '#define ', prefix, '_', vardecl.name, ' ', '_'.join(path), '\n')

def w(self, out):
  global gimported
  if self.modname not in gimported:
    mod = parser.parsemod(self.modname)
    gimported[self.modname] = mod
    _p(out, mod)
  else:
    mod = gimported[self.modname]

  # This is not correct, strictly speaking. On 'from a import X', only
  # 'X' should be in the scope, not 'a'.
  scope.current().define(mod)

  if self.alias is not None:
    _importalias(out, self.path, self.alias)
  elif self.all:
    for d in mod.imports + mod.toplevels:
      _importalias(out, self.path + [d.name], d.name)
Import.cwrite = w

def w(self, out):
  down(self.scope)
  _p(out, "#include <nlang/runtime/prelude.h>\n\n")
  ast.gmodname.append(self.name)

  for d in self.imports:
    _p(out, d, '\n\n')

  for d in self.toplevels:
    _p(out, d, '\n\n')

  ast.gmodname.pop()
  up()
Module.cwrite = w
