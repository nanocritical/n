import parser
import errors
import resolv
import copy
from ast import *

builtintypes = set(
    [Type(t) for t in 'Void U8 I8 U16 I16 U32 I32 U64 I64 Size SSize'.split()] \
    + [TypeRef('.', Type(t)) for t in 'Void U8 I8 U16 I16 U32 I32 U64 I64 Size SSize'.split()] \
    + [TypeRef('!', Type(t)) for t in 'Void U8 I8 U16 I16 U32 I32 U64 I64 Size SSize'.split()])

builtinidents = set('null'.split())

def _p(out, *args):
  assert len(args) > 0
  for a in args:
    if isinstance(a, basestring):
      out.write(a)
    elif isinstance(a, int) or isinstance(a, long):
      out.write(str(a))
    else:
      a.cwrite(out)

gscope = []
grettype = []

def _down(scope):
  global gscope
  gscope.append(scope)

def _up():
  global gscope
  assert len(gscope) > 0
  gscope.pop()


def scopedname(node):
  if (isinstance(node, Function) \
      or isinstance(node, Method)) \
      and node.name == 'main':
    return 'main'
  if isinstance(node, Type) and node in builtintypes:
    return node.name
  else:
    global gscope
    scope, _ = gscope[-1].q(node.name)
    return scope.fullcname(node)

def w(self, out):
  _p(out, scopedname(self))
Type.cwrite = w

def w(self, out):
  if self.access == '.':
    _p(out, 'nlangp__')
  else:
    _p(out, 'nlangcp__')
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

TypeGeneric.cwrite = None

def w(self, out):
  _p(out, self.decl)
ChoiceDecl.cwrite = w

def w(self, out):
  if isinstance(self.type, TypeGeneric):
    global gctx, gmodname
    for t in gctx[gmodname].typeapps[self.name]:
      t.typedecl = self
      _p(out, t)
    return

  if self.kind == TypeDecl.FORWARD:
    _p(out, 'struct ', self.type, ';\n')
    return

  _down(self.scope)
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
  _p(out, 'typedef const ', self.type, '* nlangcp__', self.type, ';\n')

  for d in self.methods:
    _p(out, d)
  for d in self.funs:
    _p(out, d)
  _up()
TypeDecl.cwrite = w

def w(self, out):
  _p(out, 'typedef struct {\n')
  for i in xrange(len(self.type.types)):
    _p(out, scopedname(self.type.types[i]), ' t', i, ';\n')
  _p(out, '}', self.type, ';\n\n')
TupleDecl.cwrite = w

def w(self, out):
  app = copy.copy(self.typedecl)
  app.type = self.type
  _p(out, app)
TypeAppDecl.cwrite = w

Union.cwrite = None

def w(self, out):
  _down(self.scope)
  global grettype
  grettype.append(self.rettype)
  if self.name[0] == '_':
    _p(out, 'static ')
  _p(out, self.rettype)
  _p(out, ' ', scopedname(self), '(')
  if isinstance(self, Method):
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
  _up()
  grettype.pop()
Method.cwrite = w
Function.cwrite = w

def w(self, out):
  _p(out, self.type, ' ', scopedname(self))
  if self.expr is not None:
    _p(out, ' = ', self.expr)
VarDecl.cwrite = w

def w(self, out):
  _p(out, self.name, '=', self.expr)
Assign.cwrite = w

def w(self, out):
  global builtinidents
  if self.name in builtinidents:
    _p(out, self.name)
  else:
    _p(out, scopedname(self))
Ident.cwrite = w

def w(self, out):
  _p(out, self.term)
Literal.cwrite = w

def w(self, out):
  _p(out, '{')
  for term in self.terms:
    _p(out, term, ', ')
  _p(out, '}')
Tuple.cwrite = w

def w(self, out):
  _p(out, self.expr)
ConstrainedExpr.cwrite = w

def w(self, out):
  _p(out, '(', self.left, ' ', self.op, ' ', self.right, ')')
BinOp.cwrite = w

def w(self, out):
  _p(out, '(', self.op, ' ', self.expr, ')')
UnOp.cwrite = w

def w(self, out):
  _p(out, self.fun, '(')
  for i in xrange(len(self.args)):
    _p(out, self.args[i])
    if i < len(self.args) - 1:
      _p(out, ',')
  _p(out, ')')
Call.cwrite = w

def w(self, out):
  global grettype
  _p(out, 'return ', '(', grettype[-1], ')', self.expr)
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
  _down(self.scope)
  _p(out, "#include <nlang/runtime/prelude.h>\n\n")
  global gmodname
  gmodname = self.name
  for t in self.ctx.tuples:
    _p(out, t)
  for d in self.imports + self.toplevels:
    _p(out, d)
    print>>out
  _up()
Module.cwrite = w
