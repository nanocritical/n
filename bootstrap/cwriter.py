import parser
import errors
import resolv
import copy
import typing
import ast
from ast import *

import scope
from scope import globalname

def _p(out, *args):
  assert len(args) > 0
  for a in args:
    if isinstance(a, basestring):
      out.write(a)
    elif isinstance(a, int) or isinstance(a, long):
      out.write(str(a))
    elif isinstance(a, Expr) and a.maybeunarycall:
      UnaryCall(a).cwrite(out)
    else:
      a.cwrite(out)

grettype = []

def wscope(self, out):
  _p(out, '_'.join(str(self).split('.')))
scope.Scope.cwrite = wscope

def wtype(self, out):
  path = self.name.split('.')
  if self.name in scope.builtintypes:
    _p(out, path[-1])
  else:
    path = path[1:]
    _p(out, '_'.join(path))
typing.Type.cwrite = wtype

def wtyperef(self, out):
  if self.access == '.':
    _p(out, 'nlangcp__')
  else:
    _p(out, 'nlangp__')
  _p(out, self.type.typecheck())
typing.TypeRef.cwrite = wtyperef
ExprTypeRef.cwrite = wtyperef

def wtypetuple(self, out):
  _p(out, 'nlangtuple__')
  for i in xrange(len(self.args)):
    _p(out, self.args[i].typecheck())
    if i < len(self.args) - 1:
      _p(out, '__')
typing.TypeTuple.cwrite = wtypetuple

def wexprchoiceargselect(self, out):
  _p(out, ExprField(ExprField(self.expr, '.', ExprValue('__unsafe_as')), '.', self.choice))
ExprChoiceArgSelect.cwrite = wexprchoiceargselect

def wexprtypeapp(self, out):
  _p(out, 'nlangapp__', globalname(self.args[0]), '__')
  for i in xrange(1, len(self.args)):
    _p(out, self.args[i])
    if i < len(self.args) - 1:
      _p(out, '__')
ExprTypeApp.cwrite = wexprtypeapp

def wtypeapp(self, out):
  _p(out, 'nlangapp__', globalname(self.defn), '__')
  for i in xrange(len(self.args)):
    _p(out, self.args[i])
    if i < len(self.args) - 1:
      _p(out, '__')
typing.TypeApp.cwrite = wtypeapp

def wtypefunction(self, out):
  pd = self.defn.scope.parent_definition.container
  if len(self.args) > 0:
    _p(out, 'nlangapp__', globalname(self.defn), '__')
    for i in xrange(len(self.args)):
      _p(out, self.args[i])
      if i < len(self.args) - 1:
        _p(out, '__')
  elif pd is not None and isinstance(pd, TypeDecl):
    _p(out, pd.typecheck(), '_', self.defn.name)
  else:
    _p(out, globalname(self.defn))
typing.TypeFunction.cwrite = wtypefunction

def wgenerictypename(self, out):
  _p(out, 'nlangapp__', globalname(self.type), '__')
  for i in xrange(len(self.args)):
    _p(out, self.args[i].typecheck())
    if i < len(self.args) - 1:
      _p(out, '_')
GenericTypename.cwrite = wgenerictypename

def w(self, out):
  raise None
typing.TypeUnboundGeneric.cwrite = w

def wintf(self, out):
  if self.unboundgeneric():
    return

  with scope.push(self.scope):
    self.mapgeninsts(lambda gen: _p(out, gen))

    for im in self.imports:
      for s in im.allnames():
        _p(out, s)

    for td in self.typedecls:
      _p(out, td)

Intf.cwrite = wintf
DynIntf.cwrite = wintf

def wchoicedecl(self, out):
  _p(out, globalname(self.defn) + '_' + self.name)
ChoiceDecl.cwrite = wchoicedecl

def wtypedecl(self, out):
  if self.unboundgeneric():
    return

  if self.kind == TypeDecl.FORWARD:
    _p(out, 'struct ', self.type, ';\n')
    return

  with scope.push(self.scope):
    if self.kind == TypeDecl.TAGGEDUNION or self.kind == TypeDecl.ENUM:
      _p(out, indent(+1), 'typedef enum {\n')
      for d in self.decls:
        if isinstance(d, ChoiceDecl):
          _p(out, indent(), d, ',\n')
      indent(-1)
      _p(out, indent(), '} nlangtag__', self.type, ';\n\n')

    self.mapgeninsts(lambda gen: _p(out, gen))

    for im in self.imports:
      for s in _imported_sources(im):
        _p(out, s)

    for td in self.typedecls:
      _p(out, td)

    _p(out, indent(+1), 'struct ', self.type ,' {\n')
    for d in self.decls:
      if not isinstance(d, ChoiceDecl):
        _p(out, indent(), d, ';\n')
    indent(-1)
    _p(out, indent(), '};\n\n')

    if self.typecheck() not in ctx().gen_instances_fwd:
      _p(out, indent(), 'typedef struct ', self.type, ' ', self.type, ';\n')
      _p(out, indent(), 'typedef ', self.type, '* nlangp__', self.type, ';\n')
      _p(out, indent(), 'typedef const ', self.type, '* nlangcp__', self.type, ';\n\n')

    for d in self.decls:
      if isinstance(d, ChoiceDecl):
        with scope.push(d.scope):
          _p(out, d.mk)
          _p(out, d.valuevar, ';\n\n')
    if self.declnum is not None:
      _p(out, self.declnum, ';\n\n')

    for d in self.methods:
      _p(out, d)
    for d in self.funs:
      _p(out, d)
TypeDecl.cwrite = wtypedecl

def wtupleinst(self, out):
  t = self.tuple.typecheck()

  global ginstantiated
  if t in ginstantiated:
    return
  ginstantiated.add(t)

  _p(out, 'struct ', t,' {\n')
  indent(+1)
  for i in xrange(len(self.tuple.args)):
    _p(out, indent(), self.tuple.args[i].typecheck(), ' t', i, ';\n')
  indent(-1)
  _p(out, '};\n\n')

  if t not in ctx().gen_instances_fwd:
    _p(out, indent(), 'typedef struct ', t, ' ', t, ';\n')
    _p(out, 'typedef ', t, '* nlangp__', t, ';\n')
    _p(out, 'typedef const ', t, '* nlangcp__', t, ';\n\n')
TupleInstance.cwrite = wtupleinst

def wunion(self, out):
  _p(out, 'union ', self.type, ' {\n')
  indent(+1);
  for f in self.fields:
    if f is not None:
      if f.type is not None:
        _p(out, indent(), '  ', f.type, ' ', f.name, ';\n')
  indent(-1)
  _p(out, indent(), '};\n\n')

  if self.typecheck() not in ctx().gen_instances_fwd:
    _p(out, indent(), 'typedef union ', self.type, ' ', self.type, ';\n')
    _p(out, 'typedef ', self.type, '* nlangp__', self.type, ';\n')
    _p(out, 'typedef const ', self.type, '* nlangcp__', self.type, ';\n\n')
Union.cwrite = wunion

def wfunctiondecl(self, out):
  if self.unboundgeneric():
    return

  with scope.push(self.scope):
    self.mapgeninsts(lambda gen: _p(out, gen))

    global grettype
    grettype.append(self.rettype.typecheck())

    if self.name[0].islower():
      _p(out, 'static ')

    if isinstance(self, MethodDecl):
      with scope.push(self.scope.parent_definition):
        _p(out, grettype[-1], '\n', self.typecheck(), '(')

      objtype = self.scope.parent_definition.container.typecheck()
      _p(out, typing.TypeRef(self.access, objtype), ' self')
      if len(self.args) > 0:
        _p(out, ', ')
    else:
      with scope.push(self.scope.parent_definition):
        _p(out, grettype[-1], '\n', self.typecheck(), '(')

    for i in xrange(len(self.args)):
      _p(out, self.args[i])
      if i != len(self.args) - 1:
        _p(out, ', ')
    if self.body is None:
      _p(out, ');\n')
    else:
      if isinstance(self.scope.parent_definition.container, TypeDecl):
        _p(out, ') {\ntypedef ',
            self.scope.parent_definition.container.typecheck(), ' this;\n',
            self.body, '}\n\n')
      else:
        _p(out, ') ', self.body, '\n\n')

  grettype.pop()
MethodDecl.cwrite = wfunctiondecl
FunctionDecl.cwrite = wfunctiondecl

def wgenericinstance(self, out):
  d = self.defn

  global ginstantiated
  if d is None or d.typecheck() in ginstantiated:
    return
  ginstantiated.add(d.typecheck())

  if isinstance(d, FunctionDecl):
    if len(d.genargs) == 0:
      # This call was not, in fact, to a generic.
      return

    _p(out, d)

  elif isinstance(d, TypeDef):
    if not isinstance(d.type, GenericTypename):
      # This call was not, in fact, to a generic.
      return

    if not isinstance(d, Intf) and not isinstance(d, DynIntf):
      _p(out, d)
GenericInstance.cwrite = wgenericinstance

def wvardecl(self, out):
  if self.mutatingblock is not None:
    _p(out, 'const ', self.typecheck(), ' ', self.name, ' = ({ ')
    _p(out, self.typecheck(), ' ', self.name)
    if self.expr is not None:
      _p(out, ' = ', self.expr)
    _p(out, ';\n', self.mutatingblock)
    _p(out, ' ', self.name, '; })')
  else:
    if self.is_meta_type():
      return
    _p(out, self.typecheck(), ' ', self.name)
    if self.expr is not None:
      _p(out, ' = ', self.expr)
VarDecl.cwrite = wvardecl

def wpattern(self, out):
  if self.is_meta_type():
    return

  for v in self.vars:
    if v is None:
      continue
    _p(out, indent(), v, ';\n')

  if self.mutatingblock is not None:
    _p(out, self.mutatingblock)
PatternDecl.cwrite = wpattern

def wfieldconstdecl(self, out):
  _p(out, 'static const ', self.vardecl.typecheck(), ' ', globalname(self))

  if self.vardecl.expr is not None:
    _p(out, ' = ', self.vardecl.expr)
  if self.vardecl.mutatingblock is not None:
    raise Exception("Unsupported")
FieldConstDecl.cwrite = wfieldconstdecl

def winitializer(self, out):
  tmp = gensym()
  _p(out, '({ ', self.expr, ' ', tmp, ';\n')
  indent(+2)
  _p(out, indent(), 'memset(&', tmp, ', 0, sizeof(', tmp, '));\n')

  typedecl = scope.current().q(self.expr).concrete_definition()
  for field, expr in self.pairs:
    found = False
    for f in typedecl.decls:
      if isinstance(f, FieldDecl) and f.name == field:
        _p(out, indent(), tmp, '.', field, ' = ', expr, ';\n')
        found = True
        break
    if not found:
      raise errors.ParseError("In initializer for type '%s', invalid field '%s', at %s" \
          % (self.expr, field, self.expr.codeloc))

  _p(out, indent(-2), tmp, '; })')
ExprInitializer.cwrite = winitializer

def wassign(self, out):
  self.typecheck()
  _p(out, self.value, ' = ', self.expr)
ExprAssign.cwrite = wassign

def wvalue(self, out):
  node = scope.current().q(self)
  if isinstance(node, VarDecl):
    _p(out, self.name)
  else:
    _p(out, node.typecheck())
ExprValue.cwrite = wvalue

def wsizeof(self, out):
  _p(out, 'sizeof')
ExprSizeof.cwrite = wsizeof

def wref(self, out):
  _p(out, '&', self.value)
ExprRef.cwrite = wref

def wderef(self, out):
  _p(out, '*', self.value)
ExprDeref.cwrite = wderef

def wexprliteral(self, out):
  expr = self.args[0]
  if isinstance(expr, basestring):
    _p(out, '"', expr, '"')
  elif isinstance(expr, bool):
    if expr:
      _p(out, '1')
    else:
      _p(out, '0')
  else:
    _p(out, expr)
ExprLiteral.cwrite = wexprliteral

def wexprnull(self, out):
  _p(out, 'null')
ExprNull.cwrite = wexprnull

def wpass(self, out):
  pass
Pass.cwrite = wpass

def wexprthis(self, out):
  _p(out, 'this')
ExprThis.cwrite = wexprthis

def wtuple(self, out):
  _p(out, '(', self.typecheck(), '){')
  for term in self.args:
    _p(out, term, ', ')
  _p(out, '}')
ExprTuple.cwrite = wtuple

def wexprtupleselect(self, out):
  type = self.typecheck()
  if isinstance(type, typing.TypeRef):
    access = '->'
  else:
    access = '.'
  _p(out, '(', self.expr, ')', access, 't', self.idx)
ExprTupleSelect.cwrite = wexprtupleselect

def wexprconstrained(self, out):
  self.typecheck()
  _p(out, '((', self.type, ')(', self.args[0], '))')
ExprConstrained.cwrite = wexprconstrained

_optrans = { 'and': '&&', 'or': '||', 'not': '!', 'neg': '-' }

def wexprbin(self, out):
  self.typecheck()
  if self.op in _optrans:
    op = _optrans[self.op]
  else:
    op = self.op
  _p(out, '(', self.args[0], ' ', op, ' ', self.args[1], ')')
ExprBin.cwrite = wexprbin

def wexprunary(self, out):
  self.typecheck()
  if self.op in _optrans:
    op = _optrans[self.op]
  else:
    op = self.op
  _p(out, '(', op, ' ', self.args[0], ')')
ExprUnary.cwrite = wexprunary

def wexprcall(self, out):
  self.typecheck()
  fundef = self.geninst.defn or scope.current().q(self.args[0]).definition()
  fun = self.geninst.defn or scope.current().q(self.args[0]).concrete_definition()

  if isinstance(self.args[0], ExprSizeof):
    assert len(self.args) == 2
    _p(out, 'sizeof(', self.args[1].typecheck(),')')
    return
  elif str(fun.scope) == '<root>.nlang.unsafe.Cast':
    with scope.push(fun.scope):
      rettype = fun.rettype.typecheck()
    _p(out, '((', rettype, ') ', self.args[1],')')
    return

  if isinstance(fundef, ChoiceDecl):
    funexpr = ExprField(self.args[0], '.', ExprValue(fundef.mk.name))
    fun = fundef.mk
  else:
    funexpr = fun.typecheck()

  if not isinstance(fun, FunctionDecl):
    return

  _p(out, funexpr, '(')
  if isinstance(fun, MethodDecl):
    assert isinstance(self.args[0], ExprField)
    xself = self.args[0].container
    deref = ''
    if not isinstance(xself.typecheck(), typing.TypeRef):
      deref = '&'
    _p(out, deref, xself)

  if len(fun.args) == 0:
    _p(out, ')')
    return

  if isinstance(fun, MethodDecl):
    _p(out, ', ')

  for i in xrange(len(fun.args)):
    if i + 1 < len(self.args):
      _p(out, self.args[i+1])
    else:
      if not fun.args[i].optionalarg:
        raise errors.ParseError("Non-optional argument '%s' is missing in call (%s), at %s" \
            % (fun.args[i], ' '.join(map(str, self.args)), self.codeloc))
      _p(out, ExprNull())

    if i < len(fun.args) - 1:
      _p(out, ', ')

  _p(out, ')')
ExprCall.cwrite = wexprcall

def wunarycall(self, out):
  self.args[0].maybeunarycall = False
  fundef = scope.current().q(self.args[0]).definition()
  fun = scope.current().q(self.args[0]).concrete_definition()

  if isinstance(fun, FunctionDecl):
    self.args[0].maybeunarycall = True
    _p(out, ExprCall(self.args[0], []))
  elif isinstance(fundef, ChoiceDecl):
    self.args[0].maybeunarycall = False  # Handled in ExprCall.cwrite().
    _p(out, ExprCall(self.args[0], []))
  else:
    _p(out, self.args[0])
UnaryCall.cwrite = wunarycall

def wexprfield(self, out):
  if hasattr(self.container, 'name') and self.container.name == '<root>':
    _p(out, self.field)

  ctype = self.container.typecheck()
  if isinstance(ctype, typing.TypeRef):
    access = '->'
  else:
    access = '.'

  if isinstance(self.container, ExprCall):
    container = TypeApp(self.args[0], *self.args[1:])
    access = '_'
  else:
    container = self.container

  if isinstance(scope.current().q(self.container), ChoiceDecl):
    access = '_'
    field = self.field.name
  elif isinstance(scope.current().q(self), ChoiceDecl):
    access = '_'
    field = self.field.name
  else:
    sc = ctype.concrete_definition().scope
    if isinstance(self.field, basestring):
      field = self.field
    else:
      field = self.field.name
    f = sc.table[field]

  _p(out, container, access, field)
ExprField.cwrite = wexprfield

def wexprreturn(self, out):
  global grettype
  typing.checkcompat(grettype[-1], self.typecheck())
  if self.expr is None:
    _p(out, 'return')
  else:
    _p(out, 'return (', self.expr, ')')
ExprReturn.cwrite = wexprreturn

def wexprcontinue(self, out):
  _p(out, 'continue;\n')
ExprContinue.cwrite = wexprcontinue

def wexprbreak(self, out):
  _p(out, 'break;\n')
ExprBreak.cwrite = wexprbreak

def wexprwhile(self, out):
  _p(out, 'while (', self.cond, ')', self.body)
  _p(out, indent(), '}\n')
ExprWhile.cwrite = wexprwhile

def wexprfor(self, out):
  with scope.push(self.scope):
    range = gensym()
    indent(+1)
    _p(out, '{\n', indent(), 'nlang_containers_IndexRange ', range, ' = ', self.iter, ';\n')
    _p(out, indent(+1), 'for (; nlang_containers_IndexRange_Next(&', range, ');) {\n')
    _p(out, indent(), self.vardecl, ' = nlang_containers_IndexRange_Index(&', range, ');\n')
    _p(out, self.body)
    indent(-1)
    _p(out, '\n', indent(-1), '}\n', indent(), '}')
ExprFor.cwrite = wexprfor
ExprPFor.cwrite = wexprfor

def wexprif(self, out):
  self.typecheck()
  _p(out, 'if (', self.condpairs[0][0], ')', self.condpairs[0][1])
  for cp in self.condpairs[1:]:
    _p(out, indent(), 'else if (', cp[0], ')', cp[1])

  if self.elsebody is not None:
    _p(out, indent(), 'else', self.elsebody)
ExprIf.cwrite = wexprif

gblockdepth = 0
def indent(delta=0):
  global gblockdepth
  r = '  ' * gblockdepth
  gblockdepth += delta
  return r

def wexprblock(self, out):
  global gblockdepth
  with scope.push(self.scope):
    _p(out, indent(+1), '{\n')
    for b in self.body:
      _p(out, indent(), b, ';\n')
    indent(-1)
    _p(out, indent(), '}')
ExprBlock.cwrite = wexprblock

def wexprmatcher(self, out):
  defn = self.match.expr.concrete_definition()
  if isinstance(defn, TypeDecl) \
      and (defn.kind == TypeDecl.TAGGEDUNION \
      or defn.kind == TypeDecl.ENUM):
    if isinstance(self.pattern, ExprCall):
      test = ExprBin('==', ExprField(self.match.exprevaltmp, '.', ExprValue('which')),
          ExprField(ExprField(defn.typecheck(), '.', self.pattern.args[0]),
            '.', ExprValue('Value')))
    else:
      test = ExprBin('==', ExprField(self.match.exprevaltmp, '.', ExprValue('which')),
          ExprField(ExprField(defn.typecheck(), '.', self.pattern),
            '.', ExprValue('Value')))
  else:
    test = ExprBin('==', self.match.exprevaltmp, self.pattern)

  with scope.push(self.scope):
    if self.first:
      _p(out, indent(), 'if (', test, ') {\n')
    else:
      _p(out, indent(), 'else if (', test, ') {\n')
    indent(+1)
    for v in self.vars:
      if v is None:
        continue
      _p(out, indent(), v, ';\n')
    indent(-1)
    _p(out, self.body, '\n', indent(), '}')
ExprMatcher.cwrite = wexprmatcher

def wexprmatch(self, out):
  with scope.push(self.scope):
    _p(out, indent(), self.varexprevaltmp, ';\n')
    first = True
    for m in self.matchers:
      m.first = first
      _p(out, m)
      first = False
    _p(out, indent(), 'else { NLANG_UNREACHED(); }')
ExprMatch.cwrite = wexprmatch

def wassert(self, out):
  _p(out, 'assert(', self.expr, ')')
Assert.cwrite = wassert

def wsemanticassert(self, out):
  pass
SemanticClaim.cwrite = wsemanticassert
SemanticAssert.cwrite = wsemanticassert

gimported = {}
ginstantiated = set()

def _importalias(im, name, alias, mod):
  if alias in ctx().importednames:
    return
  target = mod.scope.q(ast._QueryWrapper(name))
  scope.current().define(target, name=alias, noparent=True)
  im.owner.imported.append(target)
  ctx().importednames.add(alias)

def wimport(self, out):
  global gimported
  if self.modname not in gimported:
    mod = parser.parsemod(self.modname)
    gimported[self.modname] = mod
    _p(out, mod)
  else:
    mod = gimported[self.modname]

  # FIXME: This is not exactly correct: 'from a import x' should not make 'a.x'
  # available to the module. But internally we look up imported resources
  # using the full path so we need to define the module in the scope.
  scope.current().define(mod, noparent=True)

  if self.alias is not None:
    _importalias(self, self.path[-1], self.alias, mod)
  elif self.all:
    for d in mod.imported + mod.toplevels:
      _importalias(self, d.name, d.name, mod)
Import.cwrite = wimport

def wmodule(self, out):
  with scope.push(self.scope):
    _p(out, "#include <nlang/runtime/prelude.h>\n\n")
    gmodname.append(self.fullname)

    for d in self.imports:
      _p(out, d, '\n\n')

    self.firstpass()

    def forward_declare(gentype):
      if isinstance(gentype, typing.TypeFunction):
        return
      kind = 'struct'
      if isinstance(gentype, typing.Type) and isinstance(gentype.defn, Union):
        kind = 'union'
      _p(out, kind, ' ', gentype, ';\n')
      _p(out, 'typedef ', kind, ' ', gentype, ' ', gentype, ';\n')
      _p(out, 'typedef ', gentype, '* nlangp__', gentype, ';\n')
      _p(out, 'typedef const ', gentype, '* nlangcp__', gentype, ';\n')

    def forward_declare_members(gentype):
      if hasattr(gentype, 'defn') and isinstance(gentype.defn, TypeDecl):
        for f in gentype.defn.methods + gentype.defn.funs:
          tf = f.typecheck()
          if self.name[0].islower():
            _p(out, 'static ')
          _p(out, tf.rettype,' ', tf, '(')
          for i in xrange(len(tf.args)):
            _p(out, tf.args[i])
            if i < len(tf.args) - 1:
              _p(out, ', ')
          _p(out, ');\n')

    map(forward_declare, ast.ctx().gen_instances_fwd)
    _p(out, '\n')
    map(forward_declare_members, ast.ctx().gen_instances_fwd)
    _p(out, '\n')

    for d in self.toplevels:
      _p(out, d, '\n\n')

    gmodname.pop()
Module.cwrite = wmodule
