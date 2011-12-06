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
  _p(out, self.decl)
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
  if self.kind == TypeDecl.TAGGEDUNION:
    _p(out, 'typedef enum nlangtag__', self.type, '{\n')
    for d in self.decls:
      _p(out, '  nlangtag__' + scopedname(self) + d.decl.name + ',\n')
    _p(out, "  };\n")

  for gen in self.scope.gendecls:
    _p(out, gen)

  _p(out, "typedef struct {\n")
  for d in self.decls:
    _p(out, d)
    _p(out, ';\n')
  _p(out, '}', self.type, ';\n')
  _p(out, 'typedef ', self.type, '* nlangp__', self.type, ';\n')
  _p(out, 'typedef const ', self.type, '* nlangcp__', self.type, ';\n\n')

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
  applied = copy.deepcopy(self.typedecl)
  for genarg, instancearg in zip(applied.type.args, self.typeapp.args):
    genarg.instantiated = instancearg
  _p(out, applied)
TypeAppDecl.cwrite = w

Union.cwrite = None

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
      _p(out, ", ")
  for i in xrange(len(self.args)):
    _p(out, self.args[i])
    if i != len(self.args) - 1:
      _p(out, ", ")
  if self.body is None:
    _p(out, ");\n")
  else:
    _p(out, ") {\n")
    for b in self.body:
      _p(out, b, ';\n')
    _p(out, '}\n\n')
  up()
  grettype.pop()
MethodDecl.cwrite = w
FunctionDecl.cwrite = w

def w(self, out):
  instance = copy.deepcopy(self.call.funinstancedecl.fundecl)
  for arg, argexpr in zip(instance.args, self.call.terms[1:]):
    arg.type.deconstruct(instance.scope, argexpr.typecheck())
  _p(out, instance)
FunctionInstanceDecl.cwrite = w

def w(self, out):
  assert not (self.type is None and self.expr is None)
  if self.type is None:
    self.type = self.expr.typecheck()
  _p(out, self.type, ' ', scopedname(self))

  if self.expr is not None:
    typing.checkcompat(self.type, self.expr.typecheck())
    _p(out, ' = ', self.expr)
  if self.mutatingblock is not None:
    _p(out, ';\n')
    for s in self.mutatingblock:
      _p(out, s, ';\n')
VarDecl.cwrite = w

def w(self, out):
  tmp = _gensym()
  _p(out, '({ ', self.type, ' ', tmp, ';\n')
  _p(out, 'memset(&', tmp, ', 0, sizeof(', tmp, '));\n')

  typedecl = scope.current().q(self.type)
  for field, expr in self.pairs:
    found = False
    for f in typedecl.decls:
      if isinstance(f, FieldDecl) and f.name == field:
        _p(out, tmp, '.', field, ' = ', expr, ';\n')
        found = True
        break
    if not found:
      raise errors.ParseError("In initializer for type '%s', invalid field '%s', at %s" \
          % (self.type, field, self.type.codeloc))

  _p(out, tmp, '; })')
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
  _p(out, scopedname(self))
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
  _p(out, '((', self.type, ')(', self.terms[0], '))')
ConstrainedExpr.cwrite = w

def w(self, out):
  _p(out, '(', self.terms[0], ' ', self.op, ' ', self.terms[1], ')')
BinExpr.cwrite = w

def w(self, out):
  if self.op == 'neg':
    _p(out, '(- ', self.terms[0], ')')
  else:
    _p(out, '(', self.op, ' ', self.terms[0], ')')
UnExpr.cwrite = w

def w(self, out):
  if self.terms[0] == 'sizeof':
    assert len(self.terms) == 2
    _p(out, 'sizeof(', self.terms[1], ')')
    return

  _p(out, self.terms[0], '(')
  for i in xrange(1, len(self.terms)):
    _p(out, self.terms[i])
    if i < len(self.terms) - 1:
      _p(out, ',')
  _p(out, ')')
Call.cwrite = w

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
  _p(out, 'while (', self.cond, ') {')
  for b in self.body:
    _p(out, b, ';\n')
  _p(out, '}\n')
While.cwrite = w

def w(self, out):
  down(self.scope)
  range = _gensym()
  _p(out, '{\n', 'nlang_IndexRange ', range, ' = ', self.iter, ';\n')
  _p(out, 'for (; nlang_IndexRange_Next(&', range, ');) {\n')
  _p(out, self.vardecl, ' = nlang_IndexRange_Index(&', range, ');\n')
  for b in self.body:
    _p(out, b, ';\n')
  _p(out, '}\n}')
  up()
For.cwrite = w
PFor.cwrite = w

def w(self, out):
  _p(out, 'if (', self.condpairs[0][0], ') {\n')
  for b in self.condpairs[0][1]:
    _p(out, b, ';\n')
  _p(out, '}')
  for cp in self.condpairs[1:]:
    _p(out, 'else if (', cp[0], ') {\n')
    for b in cp[1]:
      _p(out, b, ';\n')
    _p(out, '}')

  if self.elsebody is not None:
    _p(out, 'else {\n',)
    for b in self.elsebody:
      _p(out, b, ';\n')
    _p(out, '}')
If.cwrite = w

def w(self, out):
  _p(out, 'assert(', self.expr, ')')
Assert.cwrite = w

def w(self, out):
  pass
SemanticClaim.cwrite = w
SemanticAssert.cwrite = w

gimported = {}

def _importalias(out, path, alias):
  vardecl = VarDecl(alias, None,
      reduce((lambda a, b: ValueField(a, '.', b)), path))
  scope.current().define(vardecl)
  prefix = re.sub(r'\.', '_', scope.current().container.name)
  _p(out, '#define ', prefix, '_', vardecl.name, ' ', '_'.join(path), '\n')

def w(self, out):
  global gimported
  if self.modname not in gimported:
    mod = parser.parsemod(self.modname)
    _p(out, mod)
    gimported[self.modname] = mod
  else:
    mod = gimported[self.modname]

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
