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

grettype = []

def w(self, out):
  _p(out, scopedname(self))
Type.cwrite = w

def w(self, out):
  if self.access == '.':
    _p(out, 'nlangcp__')
  else:
    _p(out, 'nlangp__')
  _p(out, self.type)
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
TypeGeneric.cwrite = w

def w(self, out):
  _p(out, self.decl)
ChoiceDecl.cwrite = w

def w(self, out):
  if isinstance(self.type, TypeGeneric) \
      and self.type.args[0].instantiated is None:
    for tappd in ast.gctx[ast.gmodname].typeapps[self].itervalues():
      tappd.typedecl = self
      _p(out, tappd)
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
  applied = copy.copy(self.typedecl)
  for genarg, instancearg in zip(applied.type.args, self.typeapp.args):
    genarg.instantiated = instancearg
  _p(out, applied)
TypeAppDecl.cwrite = w

Union.cwrite = None

def w(self, out):
  down(self.scope)
  global grettype
  grettype.append(self.rettype)
  if self.name[0] == '_':
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
      _p(out, b)
      _p(out, ';\n')
    _p(out, '}\n')
  up()
  grettype.pop()
MethodDecl.cwrite = w
FunctionDecl.cwrite = w

def w(self, out):
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
  _p(out, '(', self.terms[0], ')(')
  for i in xrange(1, len(self.terms)):
    _p(out, self.terms[i])
    if i < len(self.terms) - 1:
      _p(out, ',')
  _p(out, ')')
Call.cwrite = w

def w(self, out):
  global grettype
  typing.checkcompat(grettype[-1], self.expr.typecheck())
  _p(out, 'return ', '(', grettype[-1], ') (', self.expr, ')')
Return.cwrite = w

def w(self, out):
  _p(out, 'assert(', self.expr, ')')
Assert.cwrite = w

def w(self, out):
  pass
SemanticClaim.cwrite = w
SemanticAssert.cwrite = w

def w(self, out):
  mod = parser.parsemod(self.module)
  _p(out, mod)
Import.cwrite = w

def w(self, out):
  down(self.scope)
  _p(out, "#include <nlang/runtime/prelude.h>\n\n")
  ast.gmodname = self.name
  for t in self.ctx.tuples:
    _p(out, t)
  for d in self.imports + self.toplevels:
    _p(out, d)
    print>>out
  up()
Module.cwrite = w
