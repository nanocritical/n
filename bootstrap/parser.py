keywords = set('''
  type fun method union intf dynintf null let
  match except return for while continue break
  block future pfor
  import from
  and or not neg isa
'''.split())

tokens = '''
  IDENT TYPENAME NUMBER STRING
  ASSIGN
  LEQ LNE LLE LLT LGT LGE
  PLUS MINUS TIMES DIVIDE MODULO
  BWAND BWOR BWXOR RSHIFT LSHIFT
  UBWNOT
  ARROW
  EOL SOB EOB
  COMMA DOT BANG
  REFDOT REFBANG
  CTX_ASSERT CTX_SEMASSERT CTX_SEMCLAIM
  '''.split() + [kw.upper() for kw in keywords]

literals = '''( ) [ ] { } ? ~ : | *'''.split()

t_ASSIGN = r'='
t_LEQ = r'=='
t_LNE = r'!='
t_LLE = r'<='
t_LLT = r'<'
t_LGT = r'>'
t_LGE = r'>='
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_MODULO = r'%'
t_BWAND = r'&'
t_BWOR = r'\|'
t_BWXOR = r'\^'
t_UBWNOT = r'~'
t_ARROW = r'->'
t_CTX_ASSERT = r'\#\?[ ]'
t_CTX_SEMASSERT = r'\#\![ ]'
t_CTX_SEMCLAIM = r'\#~[ ]'
t_COMMA = r','
t_DOT = r'\.'
t_BANG = r'[!]'
t_REFDOT = r'\&'
t_REFBANG = r'\&\!'

def t_TYPENAME(t):
  r'_?[A-Z]\w*'
  return t

def t_IDENT(t):
  r'[A-Za-z_]\w*'
  if t.value in keywords:
    t.type = t.value.upper()
  return t

def t_NUMBER(t):
  r'''-?(?:0x[A-Fa-f0-9]+|0[0-7]+|\d+)'''
  t.value = long(t.value)
  return t

def t_STRING(t):
  r'''(?:"(?:[^"]|\\")*"|'(?:[^']|\\')*')'''
  t.value = t[1:-1]
  return t

def t_COMMENTS(t):
  r'''--.*'''
  pass

gindentation = 0

def countspacesfromend(t):
  i, n = 0, len(t.value) - 1
  while n >= 0 and t.value[n] == ' ':
    n -= 1
    i += 1

  if i % 2 != 0:
    raise errors.ParseError(
        "Indentation must be in multiple of 2, not %d, and use spaces only, line %d" \
            % (i, t.lexer.lineno))
  return i

# Trickery to handle nested blocks: In a block, statements are meant
# to be separated by EOL. But when a subblock closes, that EOL is in
# fact a EOB. So whenever a EOB is encountered, we return a EOB,
# followed by an extra EOL to satisfy the topmost statement_blocklist.
#
geobinject = None

def t_EOL(t):
  r'\s*\n[ ]*'  # Skip first blank lines.

  t.lexer.lineno += t.value.count('\n')

  global gindentation
  global geobinject
  i = countspacesfromend(t)

  if geobinject == 'EOB':
    geobinject = 'EOL'
    t.type = 'EOB'
    t.lexer.lexpos -= len(t.value)
    t.lexer.lineno -= t.value.count('\n')
    return t
  elif geobinject == 'EOL':
    geobinject = None
    gindentation -= 2
    if i != gindentation:
      geobinject = 'EOB'
      # Closing several blocks at once, so keep the token for later.
      t.lexer.lexpos -= len(t.value)
      t.lexer.lineno -= t.value.count('\n')
    return t

  if i <= gindentation - 2:
    geobinject = 'EOB'
    t.lexer.lexpos -= len(t.value)
    t.lexer.lineno -= t.value.count('\n')
    return t
  elif i == gindentation + 2:
    t.type = 'SOB'
    gindentation += 2
    return t
  elif i == gindentation:
    return t
  else:
    raise errors.ParseError(
        "Invalid indentation, expected [0..%d+2] spaces, not %d, line %d" \
            % (gindentation, i, t.lexer.lineno))

def t_SPACES(t):
  r'[ \t]'
  pass

def t_error(t):
  print>>sys.stderr, "Illegal character '%s'" % t.value[0]
  t.lexer.skip(1)

import ply.lex as lex
lex.lex()


import sys
import ast
import errors


def _define_oneof(name, *rulenames, **kw):
  def oneof(p):
    if len(p) == 1:
      p[0] = []
    else:
      p[0] = p[1]

  opt = ''
  if 'empty' in kw:
    opt = 'empty\n  |  '
  oneof.__doc__ = name + " : " + opt + '\n  | '.join(rulenames)
  globals()['p_' + name] = oneof

def _define_block(name, statement):
  def b(p):
    p[0] = p[2]
  b.__doc__ = '''%s : SOB %s_blocklist EOB''' % (name, name)
  globals()['p_' + name] = b

  def blist(p):
    if len(p) == 3:
      p[0] = [p[1]]
    else:
      p[0] = [p[1]] + p[3]
  blist.__doc__ = '''%s_blocklist : %s EOL
                                  | %s EOL %s_blocklist''' \
                                      % (name, statement, statement, name)
  globals()['p_' + name + '_blocklist'] = blist


precedence = (
    ('left', 'ASSIGN'),
    ('left', 'COMMA'),
    ('nonassoc', 'LEQ', 'LNE'),
    ('left', 'AND', 'OR'),
    ('right', 'NOT'),
    ('nonassoc', 'ISA'),
    ('nonassoc', 'LLE', 'LLT', 'LGT', 'LGE'),
    ('left', 'BWOR'),
    ('left', 'BWXOR'),
    ('left', 'BWAND'),
    ('nonassoc', 'LSHIFT', 'RSHIFT'),
    ('left', 'PLUS', 'MINUS'),
    ('nonassoc', 'DIVIDE', 'MODULO'),
    ('left', 'TIMES'),
    ('right', 'NEG'),
    ('right', 'UBWNOT'),
    ('right', 'REFDOT', 'REFBANG'),
    ('right', 'DOT', 'BANG'),
    )

def p_empty(p):
  '''empty :'''
  pass

def p_idents(p):
  '''idents : IDENT
            | IDENT idents'''
  r = [p[1]]
  if len(p) == 3:
    r += p[2]
  p[0] = r

def p_value_ident(p):
  '''value_3 : IDENT'''
  p[0] = ast.Value(p[1])

def p_value_deref(p):
  '''value_3 : IDENT DOT
             | IDENT BANG'''
  p[0] = ast.Deref(p[2], ast.Value(p[1]))

def p_value_fieldacc_bang(p):
  '''value_fieldacc : IDENT BANG IDENT
                    | IDENT DOT IDENT'''
  p[0] = p[1:4]

def p_value_fieldacc_bang_deref(p):
  '''value_fieldacc : IDENT BANG IDENT BANG
                    | IDENT DOT IDENT DOT
                    | IDENT DOT IDENT BANG'''
  p[0] = p[1:5]

def p_value_fieldacc_dot(p):
  '''value_fieldacc : value_fieldacc DOT IDENT'''
  p[0] = p[1:3] + p[3]

def p_value_fieldacc(p):
  '''value_2 : value_fieldacc'''
  deref = None
  if p[1][-1] == '.' or p[1][-1] == '!':
    deref = p[1][-1]
    p[1] = p[1][:-1]

  acclist = p[1]
  r = acclist[-1]
  for i in xrange(len(acclist) - 2, -1, -2):
    r = ast.ValueField(acclist[i-1], acclist[i], r)

  if deref is not None:
    p[0] = ast.Deref(deref, r)
  else:
    p[0] = r

def p_value_2(p):
  '''value_2 : value_3'''
  p[0] = p[1]

def p_value(p):
  '''value : value_2'''
  p[0] = p[1]

def p_type_name(p):
  '''type : TYPENAME'''
  p[0] = ast.Type(p[1])

def p_type_slice(p):
  '''type : '[' ']' type_ref'''
  p[0] = ast.TypeSlice(p[3])

def p_type_tuple_list(p):
  '''type_tuple_list : type
                     | type COMMA type_tuple_list'''
  args = [p[1]]
  if len(p) == 4:
    args += p[3]
  p[0] = args

def p_type_tuple_only(p):
  '''type_tuple : type COMMA type_tuple_list '''
  p[0] = ast.TypeTuple(p[1], *p[3])

def p_type_tuple(p):
  '''type : '(' type_tuple ')' '''
  p[0] = p[2]

def p_type_app_list(p):
  '''type_app_list : type
                   | type type_app_list'''
  args = [p[1]]
  if len(p) == 3:
    args += p[2]
  p[0] = args

def p_type_app_only(p):
  '''type_app : type type_app_list'''
  p[0] = ast.TypeApp(p[1], *p[2])

def p_type_app(p):
  '''type : '(' type_app ')' '''
  p[0] = p[2]

def p_type_ref_only(p):
  '''type_ref : DOT type %prec REFDOT
              | BANG type %prec REFBANG'''
  p[0] = ast.TypeRef(p[1], p[2])

def p_type_ref(p):
  '''type : type_ref'''
  p[0] = p[1]

def p_type_nullable(p):
  '''type : '?' type'''
  p[0] = ast.TypeRefNullable(p[2])

def p_type_top(p):
  '''type_top : type_app
              | type_tuple
              | type'''
  p[0] = p[1]

def p_expr_postfix_literal(p):
  '''expr_postfix : NUMBER
                  | STRING'''
  p[0] = ast.ExprLiteral(p[1])

def p_expr_postfix_null(p):
  '''expr_postfix : NULL'''
  p[0] = ast.ExprNull()

def p_expr_postfix_value(p):
  '''expr_postfix : value'''
  p[0] = p[1]

def p_expr_postfix_group(p):
  '''expr_postfix : '(' expr ')' '''
  p[0] = p[2]

def p_expr_unnop(p):
  '''expr : NEG expr_postfix
          | UBWNOT expr_postfix
          | NOT expr_postfix'''
  p[0] = ast.UnExpr(p[1], p[2])

def p_expr_deref(p):
  '''expr : expr_postfix DOT
          | expr_postfix BANG'''
  p[0] = ast.Deref(p[2], p[1])

def p_expr_ref(p):
  '''expr : REFDOT expr_postfix
          | REFBANG expr_postfix'''
  p[0] = ast.Ref(p[1], p[2])

def p_expr_binop(p):
  '''expr : expr PLUS expr
          | expr MINUS expr
          | expr TIMES expr
          | expr DIVIDE expr
          | expr MODULO expr
          | expr RSHIFT expr
          | expr LSHIFT expr
          | expr BWAND expr
          | expr BWOR expr
          | expr BWXOR expr
          | expr AND expr
          | expr OR expr
          | expr ISA expr
          | expr LLT expr
          | expr LLE expr
          | expr LGT expr
          | expr LGE expr
          | expr LEQ expr
          | expr LNE expr'''
  p[0] = ast.BinExpr(p[2], p[1], p[3])

def p_expr_call_list(p):
  '''expr_call_list : expr
                    | expr expr_call_list'''
  args = [p[1]]
  if len(p) == 3:
    args += p[2]
  p[0] = args

def p_expr_call_only(p):
  '''expr_call : expr expr_call_list'''
  p[0] = ast.Call(p[1], p[2])

def p_expr_call(p):
  '''expr : '(' expr_call ')' '''
  p[0] = p[2]

def p_expr_tuple_list(p):
  '''expr_tuple_list : expr
                     | expr COMMA expr_tuple_list'''
  args = [p[1]]
  if len(p) == 4:
    args += p[3]
  p[0] = args

def p_expr_tuple_only(p):
  '''expr_tuple : expr COMMA expr_tuple_list'''
  args = [p[1]] + p[3]
  p[0] = ast.Tuple(*args)

def p_expr_tuple(p):
  '''expr : '(' expr_tuple ')' '''
  p[0] = p[2]

def p_expr_constrained(p):
  '''expr : '(' expr ')' ':' type'''
  p[0] = ast.ConstrainedExpr(p[2], p[5])

def p_expr_postfix(p):
  '''expr : expr_postfix '''
  p[0] = p[1]

def p_expr_top(p):
  '''expr_top : expr_call
              | expr_tuple
              | expr'''
  p[0] = p[1]

def p_typedeclname_list(p):
  '''typedeclname_list : TYPENAME
                       | TYPENAME typedeclname_list'''
  args = [ast.TypeGenericArg(p[1])]
  if len(p) == 3:
    args += p[2]
  p[0] = args

def p_typedeclname_generic(p):
  '''typedeclname : TYPENAME typedeclname_list'''
  p[0] = ast.TypeGeneric(ast.Type(p[1]), *p[2])

def p_typedeclname(p):
  '''typedeclname : TYPENAME'''
  p[0] = ast.Type(p[1])

def p_typedident(p):
  '''typedident : IDENT ':' type'''
  p[0] = ast.VarDecl(p[1], p[3])

def p_vardecl(p):
  '''vardecl : LET typedident'''
  p[0] = p[2]

def p_vardecl_expr(p):
  '''vardecl : LET typedident ASSIGN expr_top'''
  p[2].expr = p[4]
  p[0] = p[2]

def p_vardecl_mutating(p):
  '''vardecl : LET typedident statements_block'''
  p[2].mutatingblock = p[3]
  p[0] = p[2]

def p_vardecl_expr_mutating(p):
  '''vardecl : LET typedident ASSIGN expr_top statements_block'''
  p[2].expr = p[4]
  p[2].mutatingblock = p[5]
  p[0] = p[2]

def p_fielddecl(p):
  '''fielddecl : IDENT ':' type_top'''
  p[0] = ast.FieldDecl(p[1], p[3])

def p_statement_vardecl(p):
  '''statement : vardecl'''
  p[0] = p[1]

def p_statement_assign(p):
  '''statement : value ASSIGN expr_top'''
  p[0] = ast.Assign(p[1], p[3])

def p_statement_expr(p):
  '''statement : expr_top'''
  p[0] = p[1]

def p_statement_return(p):
  '''statement : RETURN
               | RETURN expr_top'''
  if len(p) == 2:
    p[0] = ast.Return(None)
  else:
    p[0] = ast.Return(p[2])

def p_statement_assert(p):
  '''statement : CTX_ASSERT expr_top'''
  p[0] = ast.Assert(p[2])

def p_statement_semanticassert(p):
  '''statement : CTX_SEMASSERT expr_top'''
  p[0] = ast.SemanticAssert(p[2])

def p_statement_semanticclaim(p):
  '''statement : CTX_SEMCLAIM expr_top'''
  p[0] = ast.SemanticClaim(p[2])

def p_choicedecl(p):
  '''choicedecl : '|' IDENT'''
  p[0] = ast.ChoiceDecl(p[1])

def p_choicedecl_type(p):
  '''choicedecl : '|' IDENT ARROW type'''
  p[0] = ast.ChoiceDecl(p[1], p[3])

def p_typedecl_statement(p):
  '''typedecl_statement : fieldchoicedecl
                        | toplevel'''
  p[0] = p[1]

def p_typedecl(p):
  '''typedecl : TYPE typedeclname ASSIGN typedecl_block'''
  imports, typedecls, decls, methods, funs = [], [], [], [], []
  for x in p[4]:
    if isinstance(x, ast.Import):
      imports.append(x)
    elif isinstance(x, ast.TypeDecl):
      typedecls.append(x)
    elif isinstance(x, ast.ChoiceDecl) or isinstance(x, ast.FieldDecl):
      decls.append(x)
    elif isinstance(x, ast.MethodDecl):
      methods.append(x)
    else:
      funs.append(x)
  p[0] = ast.TypeDecl(p[2], [], imports, typedecls, decls, methods, funs)

def p_funargs(p):
  '''funargs : typedident
             | typedident funargs'''
  r = [p[1]]
  if len(p) == 3:
    r += p[2]
  p[0] = r

def p_funretvals(p):
  '''funretvals : typedident
                | type
                | typedident COMMA funretvals
                | type COMMA funretvals'''
  if len(p) == 2:
    p[0] = [p[1]]
  else:
    p[0] = [p[1]] + p[3]

def p_fundecl(p):
  '''fundecl : FUN IDENT ASSIGN funretvals statements_block
             | FUN IDENT funargs ASSIGN funretvals statements_block'''
  if len(p) == 6:
    p[0] = ast.FunctionDecl(p[2], [], p[4], p[5])
  else:
    p[0] = ast.FunctionDecl(p[2], p[3], p[5], p[6])

def p_methoddecl(p):
  '''methoddecl : METHOD IDENT funargs ASSIGN funretvals statements_block'''
  p[0] = ast.MethodDecl(p[2], '.', p[3], p[5], p[6])

def p_methoddecl_mutating(p):
  '''methoddecl : METHOD BANG IDENT funargs ASSIGN funretvals statements_block'''
  p[0] = ast.MethodDecl(p[3], '!', p[4], p[6], p[7])

def p_intfdecl(p):
  '''intfdecl : INTF typedeclname ASSIGN toplevels_block'''
  pass

def p_dynintfdecl(p):
  '''dynintfdecl : DYNINTF typedeclname ASSIGN toplevels_block'''
  pass

def p_modname(p):
  '''modname : IDENT
             | IDENT DOT modname'''
  if len(p) == 4:
    p[0] = '.'.join([p[1], p[3]])
  else:
    p[0] = p[1]

def p_import(p):
  '''import : IMPORT modname
            | FROM modname IMPORT '*'
            | FROM modname IMPORT idents'''
  modname = p[2]
  if len(p) == 4:
    p[0] = ast.Import(modname, modname)
  else:
    if p[4] == '*':
      p[0] = ast.Import(modname, '*')
    else:
      p[0] = [ast.Import(modname, id) for id in p[4]]

def p_toplevel(p):
  '''toplevel : import
              | typedecl
              | fundecl
              | methoddecl
              | intfdecl
              | dynintfdecl
              | vardecl'''
  p[0] = p[1]

def p_module(p):
  '''module : empty
            | toplevel EOL module'''
  if len(p) == 2:
    p[0] = []
  else:
    p[0] = [p[1]] + p[3]

_define_oneof('fieldchoicedecl', 'choicedecl', 'fielddecl', empty=True)
_define_block('statements_block', 'statement')
_define_block('toplevels_block', 'toplevel')
_define_block('typedecl_block', 'typedecl_statement')

def p_error(t):
  if t is None:
    raise errors.ParseError("Unexpected EOF")
  else:
    raise errors.ParseError("Syntax error at line %s: '%s'" % (t.lineno, t.value))

start = 'module'

import ply.yacc as yacc
yacc.yacc()


import resolv

def parsefile(modname, fn):
  with open(fn) as f:
    content = f.read()
    ast.gmodname = modname
    ast.gmodctx[modname] = ast.ModuleContext()
    ast.gctx[modname] = ast.GlobalContext()
    mod = ast.Module(yacc.parse(content, debug=0))
    mod.name = modname
    mod.ctx = ast.gmodctx[modname]
    ast.gmodname = None
    return mod

gmodcache = {}

def parsemod(modname):
  global gmodcache
  if modname in gmodcache:
    return gmodcache[modname]
  mod = parsefile(modname, resolv.find(modname))
  gmodcache[modname] = mod
  return mod
