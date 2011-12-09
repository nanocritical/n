keywords = set('''
  type fun method union intf dynintf let
  if elif else for while continue break
  match except return
  block future pfor
  import from in
  and or not neg isa
  false true null sizeof
'''.split())

tokens = '''
  IDENT NUMBER STRING
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
t_REFDOT = r'\&'
t_REFBANG = r'\&\!'

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

def t_BANG(t):
  r'''[!][=]?'''
  if len(t.value) == 2:
    t.type = 'LNE'
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
  ast.gmodctx[ast.gmodname[-1]].line = t.lexer.lineno

  global gindentation
  global geobinject
  i = countspacesfromend(t)

  if geobinject == 'EOB':
    geobinject = 'EOL'
    t.type = 'EOB'
    t.lexer.lexpos -= len(t.value)
    t.lexer.lineno -= t.value.count('\n')
    ast.gmodctx[ast.gmodname[-1]].line = t.lexer.lineno
    return t
  elif geobinject == 'EOL':
    geobinject = None
    gindentation -= 2
    if i != gindentation:
      geobinject = 'EOB'
      # Closing several blocks at once, so keep the token for later.
      t.lexer.lexpos -= len(t.value)
      t.lexer.lineno -= t.value.count('\n')
      ast.gmodctx[ast.gmodname[-1]].line = t.lexer.lineno
    return t

  if i <= gindentation - 2:
    geobinject = 'EOB'
    t.lexer.lexpos -= len(t.value)
    t.lexer.lineno -= t.value.count('\n')
    ast.gmodctx[ast.gmodname[-1]].line = t.lexer.lineno
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
  '''type : IDENT'''
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

def p_expr_postfix_truefalse(p):
  '''expr_postfix : TRUE
                  | FALSE'''
  if p[1] == "true":
    p[0] = ast.ExprLiteral(True)
  else:
    p[0] = ast.ExprLiteral(False)

def p_expr_postfix_null(p):
  '''expr_postfix : NULL'''
  p[0] = ast.ExprNull()

def p_expr_postfix_sizeof(p):
  '''expr_postfix : SIZEOF'''
  p[0] = ast.ExprSizeof()

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

def p_expr_cmpbinop(p):
  '''expr : expr LLT expr
          | expr LLE expr
          | expr LGT expr
          | expr LGE expr
          | expr LEQ expr
          | expr LNE expr'''
  p[0] = ast.CmpBinExpr(p[2], p[1], p[3])

def p_expr_boolbinop(p):
  '''expr : expr AND expr
          | expr OR expr'''
  p[0] = ast.BoolBinExpr(p[2], p[1], p[3])

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
          | expr ISA expr'''
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

def p_expr_block(p):
  '''expr : BLOCK statements_block'''

def p_expr_future(p):
  '''expr : FUTURE statements_block'''

def p_expr_initializer_pair(p):
  '''initializer_pair : IDENT ASSIGN expr'''
  p[0] = (p[1], p[3])

def p_expr_initializer_list(p):
  '''initializer_list : initializer_pair
                      | initializer_pair initializer_list'''
  if len(p) == 2:
    p[0] = [p[1]]
  else:
    p[0] = [p[1]] + p[2]

def p_expr_initializer(p):
  '''expr : IDENT '{' initializer_list '}' '''
  p[0] = ast.Initializer(ast.Type(p[1]), p[3])

def p_expr_top(p):
  '''expr_top : expr_call
              | expr_tuple
              | expr'''
  p[0] = p[1]

def p_typedeclname_list(p):
  '''typedeclname_list : IDENT
                       | IDENT typedeclname_list'''
  args = [ast.GenericArg(p[1])]
  if len(p) == 3:
    args += p[2]
  p[0] = args

def p_typedeclname_generic(p):
  '''typedeclname : IDENT typedeclname_list'''
  p[0] = ast.GenericTypename(ast.Type(p[1]), *p[2])

def p_typedeclname(p):
  '''typedeclname : IDENT'''
  p[0] = ast.Type(p[1])

def p_isalist(p):
  '''isalist : type
             | type isalist'''
  if len(p) == 2:
    p[0] = [p[1]]
  else:
    p[0] = [p[1]] + p[2]

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
  p[2].setmutatingblock(p[3])
  p[0] = p[2]

def p_vardecl_expr_mutating(p):
  '''vardecl : LET typedident ASSIGN expr_top statements_block'''
  p[2].expr = p[4]
  p[2].setmutatingblock(p[5])
  p[0] = p[2]

def p_vardecl_infer_expr(p):
  '''vardecl : LET IDENT ASSIGN expr_top'''
  var = ast.VarDecl(p[2], None)
  var.expr = p[4]
  p[0] = var

def p_vardecl_infer_expr_mutating(p):
  '''vardecl : LET IDENT ASSIGN expr_top statements_block'''
  var = VadrDecl(p[2], None)
  var.expr = p[4]
  var.setmutatingblock(p[5])
  p[0] = var

def p_fielddecl(p):
  '''fielddecl : idents ':' type_top'''
  p[0] = [ast.FieldDecl(i, p[3]) for i in p[1]]

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

def p_statement_while(p):
  '''statement : WHILE expr_top statements_block'''
  p[0] = ast.While(p[2], p[3])

def p_statement_for(p):
  '''statement : FOR typedident IN expr_top statements_block'''
  p[0] = ast.For(p[2], p[4], p[5])

def p_statement_pfor(p):
  '''statement : PFOR typedident IN expr_top statements_block'''
  p[0] = ast.PFor(p[2], p[4], p[5])

def p_statement_break(p):
  '''statement : BREAK'''
  p[0] = ast.Break()

def p_statement_continue(p):
  '''statement : CONTINUE'''
  p[0] = ast.Continue()

def p_statement_elif_list(p):
  '''elif_list : ELIF expr_top statements_block
               | ELIF expr_top statements_block EOL elif_list'''
  if len(p) == 4:
    p[0] = [(p[2], p[3])]
  else:
    p[0] = [(p[2], p[3])] + p[4]

def p_statement_if_elif_else(p):
  '''statement : IF expr_top statements_block EOL elif_list ELSE statements_block'''
  p[0] = ast.If([(p[2], p[3])] + p[5], p[7])

def p_statement_if_elif(p):
  '''statement : IF expr_top statements_block EOL elif_list'''
  p[0] = ast.If([(p[2], p[3])] + p[5], None)

def p_statement_if_else(p):
  '''statement : IF expr_top statements_block EOL ELSE statements_block'''
  p[0] = ast.If([(p[2], p[3])], p[6])

def p_statement_if(p):
  '''statement : IF expr_top statements_block'''
  p[0] = ast.If([(p[2], p[3])], None)

def p_statement_assert(p):
  '''statement : CTX_ASSERT statement'''
  p[0] = ast.Assert(p[2])

def p_statement_semanticassert(p):
  '''statement : CTX_SEMASSERT statement'''
  p[0] = ast.SemanticAssert(p[2])

def p_statement_semanticclaim(p):
  '''statement : CTX_SEMCLAIM statement'''
  p[0] = ast.SemanticClaim(p[2])

def p_choicedecl(p):
  '''choicedecl : BWOR IDENT'''
  p[0] = ast.ChoiceDecl(p[2])

def p_choicedecl_type(p):
  '''choicedecl : BWOR IDENT ARROW type'''
  p[0] = ast.ChoiceDecl(p[2], p[4])

def p_typedecl_statement(p):
  '''typedecl_statement : fieldchoicedecl
                        | toplevel'''
  p[0] = p[1]

def p_typedecl_semanticassert(p):
  '''typedecl_statement : CTX_SEMASSERT statement'''
  p[0] = ast.SemanticAssert(p[2])

def p_typedecl_semanticclaim(p):
  '''typedecl_statement : CTX_SEMCLAIM statement'''
  p[0] = ast.SemanticClaim(p[2])

def _typedef_block(block):
  imports, typedecls, decls, methods, funs, semantics = [], [], [], [], [], []
  for x in block:
    if isinstance(x, ast.Import):
      imports.append(x)
    elif isinstance(x, ast.TypeDecl):
      typedecls.append(x)
    elif isinstance(x, list):
      # That's a list of FieldDecl.
      decls.extend(x)
    elif isinstance(x, ast.ChoiceDecl) or isinstance(x, ast.FieldDecl):
      decls.append(x)
    elif isinstance(x, ast.MethodDecl):
      methods.append(x)
    elif isinstance(x, ast.FunctionDecl):
      funs.append(x)
    else:
      semantics.append(x)
  return imports, typedecls, decls, methods, funs, semantics

def p_typedecl(p):
  '''typedecl : TYPE typedeclname ASSIGN typedecl_block
              | TYPE typedeclname ASSIGN isalist typedecl_block'''
  if len(p) == 5:
    isa = []
    block = p[4]
  else:
    isa = p[4]
    block = p[5]

  imports, typedecls, decls, methods, funs, semantics = _typedef_block(block)
  p[0] = ast.TypeDecl(p[2], isa, imports, typedecls, decls, methods, funs)

def p_funargs(p):
  '''funargs : typedident
             | typedident funargs
             | '?' typedident
             | '?' typedident funargs'''
  if isinstance(p[1], ast.VarDecl):
    r = [p[1]]
    if len(p) == 3:
      r += p[2]
  else:
    r = [p[2]]
    if len(p) == 4:
      r += p[3]
    p[2].optionalarg = True
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

def p_fundecl_forward(p):
  '''fundecl : FUN IDENT ASSIGN funretvals
             | FUN IDENT funargs ASSIGN funretvals
             | '(' FUN typedeclname_list ')' IDENT ASSIGN funretvals
             | '(' FUN typedeclname_list ')' IDENT funargs ASSIGN funretvals'''
  if len(p) == 5:
    p[0] = ast.FunctionDecl(p[2], [], [], p[4], [])
  elif len(p) == 6:
    p[0] = ast.FunctionDecl(p[2], [], p[3], p[5], [])
  elif len(p) == 7:
    p[0] = ast.FunctionDecl(p[5], p[3], [], p[7], [])
  else:
    p[0] = ast.FunctionDecl(p[5], p[3], p[6], p[8], [])

def p_fundecl(p):
  '''fundecl : FUN IDENT ASSIGN funretvals statements_block
             | FUN IDENT funargs ASSIGN funretvals statements_block
             | '(' FUN typedeclname_list ')' IDENT ASSIGN funretvals statements_block
             | '(' FUN typedeclname_list ')' IDENT funargs ASSIGN funretvals statements_block'''
  if len(p) == 6:
    p[0] = ast.FunctionDecl(p[2], [], [], p[4], p[5])
  elif len(p) == 7:
    p[0] = ast.FunctionDecl(p[2], [], p[3], p[5], p[6])
  elif len(p) == 8:
    p[0] = ast.FunctionDecl(p[5], p[3], [], p[7], p[8])
  else:
    p[0] = ast.FunctionDecl(p[5], p[3], p[6], p[8], p[9])

def p_methoddecl_forward(p):
  '''methoddecl : METHOD IDENT ASSIGN funretvals
                | METHOD IDENT funargs ASSIGN funretvals
                | '(' METHOD typedeclname_list ')' IDENT ASSIGN funretvals
                | '(' METHOD typedeclname_list ')' IDENT funargs ASSIGN funretvals'''
  if len(p) == 5:
    p[0] = ast.MethodDecl(p[2], [], '.', [], p[4], [])
  elif len(p) == 6:
    p[0] = ast.MethodDecl(p[2], [], '.', p[3], p[5], [])
  elif len(p) == 7:
    p[0] = ast.MethodDecl(p[5], p[3], '.', [], p[7], [])
  else:
    p[0] = ast.MethodDecl(p[5], p[3], '.', p[6], p[8], [])

def p_methoddecl(p):
  '''methoddecl : METHOD IDENT ASSIGN funretvals statements_block
                | METHOD IDENT funargs ASSIGN funretvals statements_block
                | '(' METHOD typedeclname_list ')' IDENT ASSIGN funretvals statements_block
                | '(' METHOD typedeclname_list ')' IDENT funargs ASSIGN funretvals statements_block'''
  if len(p) == 6:
    p[0] = ast.MethodDecl(p[2], [], '.', [], p[4], p[5])
  elif len(p) == 7:
    p[0] = ast.MethodDecl(p[2], [], '.', p[3], p[5], p[6])
  elif len(p) == 8:
    p[0] = ast.MethodDecl(p[5], p[3], '.', [], p[7], p[8])
  else:
    p[0] = ast.MethodDecl(p[5], p[3], '.', p[6], p[8], p[9])

def p_methoddecl_mutating_forward(p):
  '''methoddecl : METHOD BANG IDENT ASSIGN funretvals
                | METHOD BANG IDENT funargs ASSIGN funretvals
                | '(' METHOD BANG typedeclname_list ')' IDENT ASSIGN funretvals
                | '(' METHOD BANG typedeclname_list ')' IDENT funargs ASSIGN funretvals'''
  if len(p) == 6:
    p[0] = ast.MethodDecl(p[3], [], '!', [], p[5], [])
  elif len(p) == 7:
    p[0] = ast.MethodDecl(p[3], [], '!', p[4], p[6], [])
  elif len(p) == 8:
    p[0] = ast.MethodDecl(p[6], p[4], '!', [], p[8], [])
  else:
    p[0] = ast.MethodDecl(p[6], p[4], '!', p[7], p[9], [])

def p_methoddecl_mutating(p):
  '''methoddecl : METHOD BANG IDENT ASSIGN funretvals statements_block
                | METHOD BANG IDENT funargs ASSIGN funretvals statements_block
                | '(' METHOD BANG typedeclname_list ')' IDENT ASSIGN funretvals statements_block
                | '(' METHOD BANG typedeclname_list ')' IDENT funargs ASSIGN funretvals statements_block'''
  if len(p) == 7:
    p[0] = ast.MethodDecl(p[3], [], '!', [], p[5], p[6])
  elif len(p) == 8:
    p[0] = ast.MethodDecl(p[3], [], '!', p[4], p[6], p[7])
  elif len(p) == 9:
    p[0] = ast.MethodDecl(p[6], p[4], '!', [], p[8], p[9])
  else:
    p[0] = ast.MethodDecl(p[6], p[4], '!', p[7], p[9], p[10])

def p_intfdecl(p):
  '''intfdecl : INTF typedeclname ASSIGN typedecl_block
              | INTF typedeclname ASSIGN isalist typedecl_block'''

  if len(p) == 5:
    isa = []
    block = p[4]
  else:
    isa = p[4]
    block = p[5]

  imports, typedecls, decls, methods, funs, semantics = _typedef_block(block)
  p[0] = ast.Intf(p[2], isa, imports, typedecls, decls, methods, funs)

def p_dynintfdecl(p):
  '''dynintfdecl : DYNINTF typedeclname ASSIGN typedecl_block
                 | DYNINTF typedeclname ASSIGN isalist typedecl_block'''

  if len(p) == 5:
    isa = []
    block = p[4]
  else:
    isa = p[4]
    block = p[5]

  imports, typedecls, decls, methods, funs, semantics = _typedef_block(block)
  if decls is not None or funs is not None:
    raise errors.ParseError("dynintf cannot contain these declarations: '%s'" \
        % map(str, decls + funs))
  p[0] = ast.DynIntf(p[2], isa, imports, typedecls, methods)

def p_modname(p):
  '''modname : IDENT
             | IDENT DOT modname'''
  if len(p) == 2:
    p[0] = [p[1]]
  else:
    p[0] = [p[1]] + p[3]

def p_import(p):
  '''import : IMPORT modname
            | FROM modname IMPORT '*'
            | FROM modname IMPORT idents'''
  path = p[2]
  if len(p) == 3:
    p[0] = ast.Import(path)
  else:
    if p[4] == '*':
      p[0] = ast.Import(path, all=True)
    else:
      p[0] = [ast.Import(path + [id], alias=id) for id in p[4]]

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
    if isinstance(p[1], list):
      p[0] = p[1] + p[3]
    else:
      p[0] = [p[1]] + p[3]

_define_oneof('fieldchoicedecl', 'choicedecl', 'fielddecl', empty=True)
_define_block('statements_block', 'statement')
_define_block('typedecl_block', 'typedecl_statement')

def p_error(t):
  if t is None:
    raise errors.ParseError("Unexpected EOF: %s" \
        % (ast.gmodctx[ast.gmodname[-1]].fn))
  else:
    raise errors.ParseError("Syntax error at: %s:%s: '%s'" \
        % (ast.gmodctx[ast.gmodname[-1]].fn, t.lineno, t.value))

start = 'module'

import ply.yacc as yacc

import resolv

def parsefile(modname, fn):
  with open(fn) as f:
    lex.lex()
    yacc.yacc(outputdir='bootstrap')
    content = f.read()
    ast.gmodname.append(modname)
    ast.gmodctx[modname] = ast.ModuleContext(modname, fn)
    mod = ast.Module(yacc.parse(content, debug=0))
    mod.name = modname
    mod.ctx = ast.gmodctx[modname]
    ast.gmodname.pop()
    return mod

gmodcache = {}

def parsemod(modname):
  global gmodcache
  if modname in gmodcache:
    return gmodcache[modname]

  mod = parsefile(modname, resolv.find(modname))
  gmodcache[modname] = mod
  return mod
