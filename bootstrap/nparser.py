import ply.yacc as yacc
import resolv
import ply.lex as lex
import sys
import ast
import typing
import errors

gfn = None

keywords = set('''
  type fun method union intf dyn let
  if elif else for while continue break
  match except return
  block future pfor
  import from inherit in declare
  and or not isa
  false true null sizeof this
  pass
'''.split())

tokens = '''
  IDENT NUMBER STRING
  ASSIGN
  LEQ LNE LLE LLT LGT LGE
  PLUS MINUS TIMES DIVIDE MODULO
  BWAND BWOR BWXOR RSHIFT LSHIFT
  UBWNOT
  ARROW
  EOL SOB EOB COLON
  COMMA DOT BANG
  REFDOT REFBANG POSTDOT POSTBANG
  SLICEBRAKETS
  LINIT RINIT
  CTX_ASSERT CTX_SEMASSERT CTX_SEMCLAIM
  '''.split() + [kw.upper() for kw in keywords]

literals = '''( ) [ ] ? ~ | *'''.split()

t_ASSIGN = r'='
t_LEQ = r'=='
t_LLE = r'<='
t_LSHIFT = r'<<'
t_RSHIFT = r'>>'
t_LLT = r'<'
t_LGT = r'>'
t_LGE = r'>='
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_MODULO = r'%'
t_BWOR = r'\|'
t_BWXOR = r'\^'
t_BWAND = r'&'
t_UBWNOT = r'~'
t_ARROW = r'->'
t_CTX_ASSERT = r'\#\?[ ]'
t_CTX_SEMASSERT = r'\#\![ ]'
t_CTX_SEMCLAIM = r'\#~[ ]'
t_COMMA = r','
t_COLON = r':'
t_REFDOT = r'@'
t_REFBANG = r'@\!'
t_SLICEBRAKETS = r'\[\]'
t_LINIT = r'{{'
t_RINIT = r'}}'

def t_IDENT(t):
  r'[A-Za-z_]\w*'
  if t.value in keywords:
    t.type = t.value.upper()
  return t

def t_NUMBER(t):
  r'''(?:0x[A-Fa-f0-9]+|0[0-7]+|\d+)'''
  t.value = long(t.value, 0)
  return t

def t_STRING(t):
  r'''(?:"(?:[^"]|\\")*"|'(?:[^']|\\')*')'''
  t.value = t.value[1:-1]
  return t

# POST{DOT,BANG} are used for this family of cases:
#   object \
#      .method a b. c!
# Not to be parsed as: (object.) method a (b.c)!
#
# The grammar is ambiguous wrt DOT and BANG when used for field access and
# derefencing, mixed in with function calls, in the presence of spaces.  But we
# really feel it's worth it in terms of practical syntax, so we rely on the
# lexer to make the grammar (a bit) space-sensitive.
#
# '\S[!.]\s+' -> POST{DOT,BANG}
# '\S[!.]\S' -> {DOT,BANG}
#
# Dereferencing can only be DOT or POSTDOT.
# Field access can only be DOT.

def t_DOT(t):
  r'\s?\.\s?'
  before, after = t.value[0].isspace(), t.value[-1].isspace()
  if not before and after:
    t.type = 'POSTDOT'
    t.lexer.lexpos -= 1
  else:
    t.type = 'DOT'
  t.value = '.'  # Normalize.
  return t

def t_BANG(t):
  r'''\s?[!][\s=]?'''
  if '=' in t.value:
    t.type = 'LNE'
    t.value = '!='
    return t

  before, after = t.value[0].isspace(), t.value[-1].isspace()
  if not before and after:
    t.type = 'POSTBANG'
    t.lexer.lexpos -= 1
  else:
    t.type = 'BANG'
  t.value = '!'  # Normalize.
  return t

gindentation = 0

def countspacesfromend(t):
  i, n = 0, len(t.value) - 1
  while n >= 0 and t.value[n] == ' ':
    n -= 1
    i += 1

  if i % 2 != 0:
    raise errors.ParseError(
        "Indentation must be in multiple of 2, not %d, and use spaces only, %s:%d" \
            % (i, gfn, t.lexer.lineno))
  return i

# Trickery to handle nested blocks: In a block, statements are meant
# to be separated by EOL. But when a subblock closes, that EOL is in
# fact a EOB. So whenever a EOB is encountered, we return a EOB,
# followed by an extra EOL to satisfy the topmost statement_blocklist.
#
geobinject = None

def t_EOL(t):
  r'\s*(\n|--.*\n)+[ ]*'  # Skip first blank lines.

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
        "Invalid indentation, expected [0..%d+2] spaces, not %d, %s:%d" \
            % (gindentation, i, gfn, t.lexer.lineno))

def t_SPACES(t):
  r'[ \t]'
  pass

def t_error(t):
  print>>sys.stderr, "Illegal character '%s'" % t.value[0]
  t.lexer.skip(1)

lex.lex()


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

def _define_block(name, statement, *statements_witheol):
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

  # To resolve shift/reduce ambiguities, we sometime had to add the EOL
  # in the statement rule: these statements are handled specially below.
  i = 1
  for s in statements_witheol:
    def blist(p):
      if len(p) == 2:
        p[0] = [p[1]]
      else:
        p[0] = [p[1]] + p[2]
    blist.__doc__ = '''%s_blocklist : %s
                                    | %s %s_blocklist''' \
                                        % (name, s, s, name)
    globals()['p_' + name + ('_blocklist_%d' % i)] = blist
    i += 1


precedence = (
    ('left', 'ASSIGN'),
    ('left', 'COMMA'),
    ('left', 'AND', 'OR'),
    ('right', 'NOT'),
    ('nonassoc', 'ISA'),
    ('nonassoc', 'LLE', 'LLT', 'LGT', 'LGE', 'LEQ', 'LNE'),
    ('left', 'BWOR'),
    ('left', 'BWXOR'),
    ('left', 'BWAND'),
    ('nonassoc', 'LSHIFT', 'RSHIFT'),
    ('left', 'PLUS', 'MINUS'),
    ('nonassoc', 'DIVIDE', 'MODULO'),
    ('left', 'TIMES'),
    ('right', 'UBWNOT'),
    ('right', 'REFDOT', 'REFBANG'),
    ('left', 'COLON'),
    ('left', 'DOT', 'BANG'),
    ('right', 'POSTDOT', 'POSTBANG'),
    ('right', 'IDENT'),
    )

def p_empty(p):
  '''empty :'''
  pass

def p_idents(p):
  '''idents : IDENT
            | IDENT idents'''
  r = [ast.ExprValue(p[1])]
  if len(p) == 3:
    r += p[2]
  p[0] = r

def p_value_ident(p):
  '''value_3 : IDENT'''
  p[0] = ast.ExprValue(p[1])
  p[0].maybeunarycall = True

def p_value_2(p):
  '''value_2 : value_3'''
  p[0] = p[1]

def p_value(p):
  '''value : value_2'''
  p[0] = p[1]

def p_type_name(p):
  '''type_postfix : IDENT'''
  p[0] = ast.ExprValue(p[1])

def p_type_this(p):
  '''type_postfix : THIS'''
  p[0] = ast.ExprThis()

def p_type_postfix_top(p):
  '''type_postfix : '(' type_top ')' '''
  p[0] = p[2]

def p_type_postfix(p):
  '''type : type_postfix'''
  p[0] = p[1]

def p_type_field(p):
  '''type_postfix : type_postfix DOT type_postfix
                  | type_postfix BANG type_postfix'''
  p[0] = ast.ExprField(p[1], p[2], p[3])

def p_type_deref(p):
  '''type : type_postfix DOT
          | type_postfix BANG
          | type_postfix POSTDOT
          | type_postfix POSTBANG'''
  p[0] = ast.ExprDeref(p[2], p[1])

def p_type_slice(p):
  '''type_postfix : SLICEBRAKETS type_postfix'''
  p[0] = ast.ExprTypeSlice(p[2])

def p_type_slice_sized(p):
  '''type_postfix : '[' expr_top ']' type_postfix'''
  p[0] = ast.ExprTypeSliceSized(p[4], p[2])

def p_type_tuple_list(p):
  '''type_tuple_list : type
                     | type COMMA type_tuple_list'''
  args = [p[1]]
  if len(p) == 4:
    args += p[3]
  p[0] = args

def p_type_tuple_only(p):
  '''type_tuple : type COMMA type_tuple_list '''
  p[0] = ast.ExprTuple(p[1], *p[3])

def p_type_app_list(p):
  '''type_app_list : type
                   | type type_app_list'''
  args = [p[1]]
  if len(p) == 3:
    args += p[2]
  p[0] = args

def p_type_app_only(p):
  '''type_app : type type_app_list'''
  p[0] = ast.ExprCall(p[1], p[2])

def p_type_ref_only(p):
  '''type_ref : REFDOT type_postfix
              | REFBANG type_postfix'''
  if p[1] == '@':
    p[0] = ast.ExprRef(p[2])
  else:
    p[0] = ast.ExprMutableRef(p[2])

def p_type_ref_nullable(p):
  '''type_ref : '?' REFDOT type_postfix
              | '?' REFBANG type_postfix'''
  if p[2] == '@':
    p[0] = ast.ExprNullableRef(p[3])
  else:
    p[0] = ast.ExprNullableMutableRef(p[3])

def p_type_ref(p):
  '''type : type_ref'''
  p[0] = p[1]

def p_type_top(p):
  '''type_top : type_app
              | type_tuple
              | type'''
  p[0] = p[1]

def p_expr_postfix_literal_number(p):
  '''expr_postfix : NUMBER'''
  p[0] = ast.ExprLiteral(p[1])

def p_expr_postfix_string(p):
  '''expr_postfix : STRING'''
  p[0] = ast.ExprLiteral(p[1].decode('string-escape'))

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

def p_expr_postfix_this(p):
  '''expr_postfix : THIS'''
  p[0] = ast.ExprThis()

def p_expr_postfix_value(p):
  '''expr_postfix : value'''
  p[0] = p[1]

def p_expr_postfix_group(p):
  '''expr_postfix : '(' expr_top ')' '''
  p[0] = p[2]

def p_expr_unnop(p):
  '''expr : UBWNOT expr_postfix'''
  p[0] = ast.ExprUnary(p[1], p[2])

def p_expr_constrained(p):
  '''expr_postfix : expr_postfix COLON type'''
  p[0] = ast.ExprConstrained(p[1], p[3])

def p_expr_ref(p):
  '''expr : REFDOT expr_postfix
          | REFBANG expr_postfix'''
  if p[1] == '@':
    p[0] = ast.ExprRef(p[2])
  else:
    p[0] = ast.ExprMutableRef(p[2])

def p_expr_ref_nullable(p):
  '''expr : '?' REFDOT expr_postfix
          | '?' REFBANG expr_postfix'''
  if p[2] == '@':
    p[0] = ast.ExprNullableRef(p[3])
  else:
    p[0] = ast.ExprNullableMutableRef(p[3])

def p_expr_field(p):
  '''expr_postfix : expr_postfix DOT value
                  | expr_postfix BANG value'''
  p[0] = ast.ExprField(p[1], p[2], p[3])
  p[3].maybeunarycall = False
  p[0].maybeunarycall = True

def p_expr_element(p):
  '''expr_postfix : expr_postfix DOT '[' expr_top ']'
                  | expr_postfix BANG '[' expr_top ']' '''
  p[0] = ast.ExprFieldGetElement(p[1], p[2], p[4])
  p[0].maybeunarycall = True

def p_expr_deref(p):
  '''expr : expr_postfix DOT
          | expr_postfix BANG
          | expr_postfix POSTDOT
          | expr_postfix POSTBANG'''
  p[0] = ast.ExprDeref(p[2], p[1])

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
  p[0] = ast.ExprBin(p[2], p[1], p[3])

def p_expr_call_list(p):
  '''expr_call_list : expr
                    | expr expr_call_list'''
  args = [p[1]]
  if len(p) == 3:
    args += p[2]
  p[0] = args

def p_expr_call_only(p):
  '''expr_call : expr expr_call_list'''
  p[1].maybeunarycall = False
  p[0] = ast.ExprCall(p[1], p[2])
  p[0].maybeunarycall = True  # If this is in fact a generic type instantiation.

def p_expr_postfix(p):
  '''expr : expr_postfix'''
  p[0] = p[1]

def p_expr_block(p):
  '''expr : BLOCK statements_block'''
  p[0] = p[2]

def p_expr_future(p):
  '''expr : FUTURE statements_block'''
  p[0] = p[2]

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
  '''expr : expr_postfix LINIT RINIT
          | expr_postfix LINIT initializer_list RINIT'''
  if len(p) == 4:
    p[0] = ast.ExprInitializer(p[1], [])
  else:
    p[0] = ast.ExprInitializer(p[1], p[3])

def p_expr_top__1(p):
  '''expr_top__1 : expr
                 | expr_call'''
  p[0] = p[1]

def p_expr_top__1_unary(p):
  '''expr_top__1 : MINUS expr_top__1'''
  p[0] = ast.ExprUnary(p[1], p[2])

def p_expr_top__2(p):
  '''expr_top__2 : expr_top__1 LLT expr_top__1
                 | expr_top__1 LLE expr_top__1
                 | expr_top__1 LGT expr_top__1
                 | expr_top__1 LGE expr_top__1
                 | expr_top__1 LEQ expr_top__1
                 | expr_top__1 LNE expr_top__1'''
  p[0] = ast.ExprCmpBin(p[2], p[1], p[3])

def p_expr_top__12(p):
  '''expr_top__12 : expr_top__1
                  | expr_top__2'''
  p[0] = p[1]

def p_expr_top__12_unary(p):
  '''expr_top__12 : NOT expr_top__12'''
  p[0] = ast.ExprUnary(p[1], p[2])

def p_expr_top__3(p):
  '''expr_top__3 : expr_top__12 AND expr_top__12
                 | expr_top__12 OR expr_top__12'''
  p[0] = ast.ExprBoolBin(p[2], p[1], p[3])

def p_expr_top__123(p):
  '''expr_top__123 : expr_top__12
                   | expr_top__3'''
  p[0] = p[1]

def p_expr_tuple_list(p):
  '''expr_tuple_list : expr_top__123
                     | expr_top__123 COMMA expr_tuple_list'''
  args = [p[1]]
  if len(p) == 4:
    args += p[3]
  p[0] = args

def p_expr_top__4(p):
  '''expr_top__4 : expr_top__123 COMMA expr_tuple_list'''
  args = [p[1]] + p[3]
  p[0] = ast.ExprTuple(*args)

def p_expr_top(p):
  '''expr_top : expr_top__12
              | expr_top__3
              | expr_top__4'''
  p[0] = p[1]

def p_typedeclname_list(p):
  '''typedeclname_list : IDENT
                       | IDENT typedeclname_list
                       | IDENT COLON type
                       | IDENT COLON type typedeclname_list'''
  if len(p) == 2:
    p[0] = [ast.GenericArg(p[1])]
  elif len(p) == 3:
    p[0] = [ast.GenericArg(p[1])] + p[2]
  elif len(p) == 4:
    p[0] = [ast.GenericArg(p[1], p[3])]
  elif len(p) == 5:
    p[0] = [ast.GenericArg(p[1], p[3])] + p[4]

def p_typedeclname_generic(p):
  '''typedeclname : IDENT typedeclname_list'''
  p[0] = ast.GenericTypename(ast.ExprValue(p[1]), *p[2])

def p_typedeclname(p):
  '''typedeclname : IDENT'''
  p[0] = ast.ExprValue(p[1])

def p_isalist(p):
  '''isalist : type
             | type isalist'''
  if len(p) == 2:
    p[0] = [p[1]]
  else:
    p[0] = [p[1]] + p[2]

def p_typedident(p):
  '''typedident : IDENT COLON type'''
  p[0] = ast.VarDecl(ast.ExprConstrained(ast.ExprValue(p[1]), p[3]))

def p_pattern(p):
  '''pattern : LET expr_top'''
  p[0] = ast.PatternDecl(p[2])

def p_pattern_expr(p):
  '''pattern : LET expr_top ASSIGN expr_top'''
  p[0] = ast.PatternDecl(p[2], p[4])

def p_pattern_mutating(p):
  '''pattern : LET expr_top statements_block'''
  pat = ast.PatternDecl(p[2])
  pat.setmutatingblock(p[3])
  p[0] = pat

def p_pattern_expr_mutating(p):
  '''pattern : LET expr_top ASSIGN expr_top statements_block'''
  pat = ast.PatternDecl(p[2], p[4])
  pat.setmutatingblock(p[5])
  p[0] = pat

def p_static_pattern(p):
  '''static_pattern : pattern'''
  p[0] = p[1]
  p[0].static = True

def p_fielddecl(p):
  '''fielddecl : idents COLON type_top'''
  p[0] = [ast.FieldDecl(None, i, p[3]) for i in p[1]]

def p_statement_pass(p):
  '''statement : PASS'''
  p[0] = ast.Pass()

def p_statement_pattern(p):
  '''statement : pattern'''
  p[0] = p[1]

def p_statement_assign(p):
  '''statement : expr_top ASSIGN expr_top'''
  if isinstance(p[1], ast.ExprFieldGetElement):
    p[1].args[0].field = ast.ExprValue('operator_set__')
    p[1].maybeunarycall = False
    p[1].args.append(p[3])
    p[0] = p[1]
  else:
    p[0] = ast.ExprAssign(p[1], p[3])

def p_statement_expr(p):
  '''statement : expr_top'''
  p[0] = p[1]

def p_statement_return(p):
  '''statement : RETURN
               | RETURN expr_top'''
  if len(p) == 2:
    p[0] = ast.ExprReturn(None)
  else:
    p[0] = ast.ExprReturn(p[2])

def p_statement_while(p):
  '''statement : WHILE expr_top statements_block'''
  p[0] = ast.ExprWhile(p[2], p[3])

def p_statement_for(p):
  '''statement : FOR expr_top IN expr_top statements_block'''
  p[0] = ast.ExprFor(p[2], p[4], p[5])

def p_statement_pfor(p):
  '''statement : PFOR expr_top IN expr_top statements_block'''
  p[0] = ast.ExprPFor(p[2], p[4], p[5])

def p_statement_break(p):
  '''statement : BREAK'''
  p[0] = ast.ExprBreak()

def p_statement_continue(p):
  '''statement : CONTINUE'''
  p[0] = ast.ExprContinue()

def p_statement_elif_list(p):
  '''elif_list : ELIF expr_top statements_block EOL
               | ELIF expr_top statements_block EOL elif_list'''
  if len(p) == 5:
    p[0] = [(p[2], p[3])]
  else:
    p[0] = [(p[2], p[3])] + p[5]

def p_statement_if_elif_else(p):
  '''statement : IF expr_top statements_block EOL elif_list ELSE statements_block'''
  p[0] = ast.ExprIf([(p[2], p[3])] + p[5], p[7])

def p_statement_if_elif(p):
  '''statement_witheol : IF expr_top statements_block EOL elif_list'''
  p[0] = ast.ExprIf([(p[2], p[3])] + p[5], None)

def p_statement_if_else(p):
  '''statement : IF expr_top statements_block EOL ELSE statements_block'''
  p[0] = ast.ExprIf([(p[2], p[3])], p[6])

def p_statement_if(p):
  '''statement_witheol : IF expr_top statements_block EOL'''
  p[0] = ast.ExprIf([(p[2], p[3])], None)

def p_matchers(p):
  '''matcher : BWOR expr_top statements_block'''
  p[0] = ast.ExprMatcher(p[2], p[3])

def p_match(p):
  '''statement : MATCH expr_top matchers_block'''
  p[0] = ast.ExprMatch(p[2], p[3])

def p_statement_assert(p):
  '''statement : CTX_ASSERT statement'''
  p[0] = ast.Assert(p[2])

def p_statement_semanticassert(p):
  '''statement : CTX_SEMASSERT statement'''
  p[0] = ast.SemanticAssert(p[2])

def p_statement_semanticclaim(p):
  '''statement : CTX_SEMCLAIM statement'''
  p[0] = ast.SemanticClaim(p[2])

def p_statements_block(p):
  '''statements_block : _statements_block'''
  p[0] = ast.ExprBlock(p[1])

def p_choicedecl(p):
  '''choicedecl : BWOR IDENT'''
  p[0] = ast.ChoiceDecl(p[2])

def p_choicedecl_value(p):
  '''choicedecl : BWOR IDENT ASSIGN expr_top'''
  p[0] = ast.ChoiceDecl(p[2], value=p[4])

def p_choicedecl_type(p):
  '''choicedecl : BWOR IDENT type'''
  p[0] = ast.ChoiceDecl(p[2], typearg=p[3])

def p_choicedecl_type_value(p):
  '''choicedecl : BWOR IDENT type ASSIGN expr_top'''
  p[0] = ast.ChoiceDecl(p[2], typearg=p[3], value=p[5])

def p_typedecl_statement(p):
  '''typedecl_statement : inherit
                        | fieldchoicedecl
                        | typedecl
                        | fundecl
                        | methoddecl
                        | intfdecl
                        | pattern'''
  p[0] = p[1]

def p_typedecl_semanticassert(p):
  '''typedecl_statement : CTX_SEMASSERT statement'''
  p[0] = ast.SemanticAssert(p[2])

def p_typedecl_semanticclaim(p):
  '''typedecl_statement : CTX_SEMCLAIM statement'''
  p[0] = ast.SemanticClaim(p[2])

def p_inherit(p):
  '''inherit : INHERIT expr_top
             | FROM expr_top INHERIT expr_top'''
  path = p[2]
  if len(p) == 3:
    p[0] = ast.Inherit(path)
  else:
    raise 'Unsupported: from expr_top inherit intf'

def _typedef_block(block):
  inherits, typedecls, decls, methods, funs, semantics = [], [], [], [], [], []
  for x in block:
    if isinstance(x, ast.Inherit):
      inherits.append(x)
    elif isinstance(x, ast.TypeDecl):
      typedecls.append(x)
    elif isinstance(x, list):
      # That's a list of FieldDecl.
      decls.extend(x)
    elif isinstance(x, ast.ChoiceDecl) or isinstance(x, ast.FieldDecl) or isinstance(x, ast.PatternDecl):
      decls.append(x)
    elif isinstance(x, ast.MethodDecl):
      methods.append(x)
    elif isinstance(x, ast.FunctionDecl):
      funs.append(x)
    else:
      semantics.append(x)
  return inherits, typedecls, decls, methods, funs, semantics

def p_typedecl_empty(p):
  '''typedecl : TYPE typedeclname ASSIGN
              | TYPE typedeclname ASSIGN isalist
              | '(' TYPE typedeclname_list ')' typedeclname ASSIGN
              | '(' TYPE typedeclname_list ')' typedeclname ASSIGN isalist'''
  if len(p) == 4:
    name = p[2]
    isa = []
    genargs = []
  elif len(p) == 5:
    name = p[2]
    isa = p[4]
    genargs = []
  elif len(p) == 7:
    name = p[5]
    isa = []
    genargs = p[3]
  elif len(p) == 8:
    name = p[5]
    isa = p[7]
    genargs = p[3]
  p[0] = ast.TypeDecl(name, genargs, isa, [], [], [], [], [])

def p_typedecl(p):
  '''typedecl : TYPE typedeclname ASSIGN typedecl_block
              | TYPE typedeclname ASSIGN isalist typedecl_block
              | '(' TYPE typedeclname_list ')' typedeclname ASSIGN typedecl_block
              | '(' TYPE typedeclname_list ')' typedeclname ASSIGN isalist typedecl_block'''
  if len(p) == 5:
    name = p[2]
    isa = []
    genargs = []
    block = p[4]
  elif len(p) == 6:
    name = p[2]
    isa = p[4]
    genargs = []
    block = p[5]
  elif len(p) == 8:
    name = p[5]
    isa = []
    genargs = p[3]
    block = p[7]
  elif len(p) == 9:
    name = p[5]
    isa = p[7]
    genargs = p[3]
    block = p[8]

  inherits, typedecls, decls, methods, funs, semantics = _typedef_block(block)
  p[0] = ast.TypeDecl(name, genargs, isa, inherits, typedecls, decls, methods, funs)

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
    p[0] = ast.FunctionDecl(p[2], [], [], p[4], None)
  elif len(p) == 6:
    p[0] = ast.FunctionDecl(p[2], [], p[3], p[5], None)
  elif len(p) == 8:
    p[0] = ast.FunctionDecl(p[5], p[3], [], p[7], None)
  else:
    p[0] = ast.FunctionDecl(p[5], p[3], p[6], p[8], None)

def p_fundecl(p):
  '''fundecl : FUN IDENT ASSIGN funretvals statements_block
             | FUN IDENT funargs ASSIGN funretvals statements_block
             | '(' FUN typedeclname_list ')' IDENT ASSIGN funretvals statements_block
             | '(' FUN typedeclname_list ')' IDENT funargs ASSIGN funretvals statements_block'''
  if len(p) == 6:
    p[0] = ast.FunctionDecl(p[2], [], [], p[4], p[5])
  elif len(p) == 7:
    p[0] = ast.FunctionDecl(p[2], [], p[3], p[5], p[6])
  elif len(p) == 9:
    p[0] = ast.FunctionDecl(p[5], p[3], [], p[7], p[8])
  else:
    p[0] = ast.FunctionDecl(p[5], p[3], p[6], p[8], p[9])

def p_methoddecl_forward(p):
  '''methoddecl : METHOD IDENT ASSIGN funretvals
                | METHOD IDENT funargs ASSIGN funretvals
                | '(' METHOD typedeclname_list ')' IDENT ASSIGN funretvals
                | '(' METHOD typedeclname_list ')' IDENT funargs ASSIGN funretvals'''
  if len(p) == 5:
    p[0] = ast.MethodDecl(p[2], [], '.', [], p[4], None)
  elif len(p) == 6:
    p[0] = ast.MethodDecl(p[2], [], '.', p[3], p[5], None)
  elif len(p) == 8:
    p[0] = ast.MethodDecl(p[5], p[3], '.', [], p[7], None)
  else:
    p[0] = ast.MethodDecl(p[5], p[3], '.', p[6], p[8], None)

def p_methoddecl(p):
  '''methoddecl : METHOD IDENT ASSIGN funretvals statements_block
                | METHOD IDENT funargs ASSIGN funretvals statements_block
                | '(' METHOD typedeclname_list ')' IDENT ASSIGN funretvals statements_block
                | '(' METHOD typedeclname_list ')' IDENT funargs ASSIGN funretvals statements_block'''
  if len(p) == 6:
    p[0] = ast.MethodDecl(p[2], [], '.', [], p[4], p[5])
  elif len(p) == 7:
    p[0] = ast.MethodDecl(p[2], [], '.', p[3], p[5], p[6])
  elif len(p) == 9:
    p[0] = ast.MethodDecl(p[5], p[3], '.', [], p[7], p[8])
  else:
    p[0] = ast.MethodDecl(p[5], p[3], '.', p[6], p[8], p[9])

def p_methoddecl_mutating_forward(p):
  '''methoddecl : METHOD POSTBANG IDENT ASSIGN funretvals
                | METHOD POSTBANG IDENT funargs ASSIGN funretvals
                | '(' METHOD POSTBANG typedeclname_list ')' IDENT ASSIGN funretvals
                | '(' METHOD POSTBANG typedeclname_list ')' IDENT funargs ASSIGN funretvals'''
  if len(p) == 6:
    p[0] = ast.MethodDecl(p[3], [], '!', [], p[5], None)
  elif len(p) == 7:
    p[0] = ast.MethodDecl(p[3], [], '!', p[4], p[6], None)
  elif len(p) == 9:
    p[0] = ast.MethodDecl(p[6], p[4], '!', [], p[8], None)
  else:
    p[0] = ast.MethodDecl(p[6], p[4], '!', p[7], p[9], None)

def p_methoddecl_mutating(p):
  '''methoddecl : METHOD POSTBANG IDENT ASSIGN funretvals statements_block
                | METHOD POSTBANG IDENT funargs ASSIGN funretvals statements_block
                | '(' METHOD POSTBANG typedeclname_list ')' IDENT ASSIGN funretvals statements_block
                | '(' METHOD POSTBANG typedeclname_list ')' IDENT funargs ASSIGN funretvals statements_block'''
  if len(p) == 7:
    p[0] = ast.MethodDecl(p[3], [], '!', [], p[5], p[6])
  elif len(p) == 8:
    p[0] = ast.MethodDecl(p[3], [], '!', p[4], p[6], p[7])
  elif len(p) == 10:
    p[0] = ast.MethodDecl(p[6], p[4], '!', [], p[8], p[9])
  else:
    p[0] = ast.MethodDecl(p[6], p[4], '!', p[7], p[9], p[10])

def p_intfdecl_empty(p):
  '''intfdecl : INTF typedeclname ASSIGN
              | INTF typedeclname ASSIGN isalist
              | '(' INTF typedeclname_list ')' typedeclname ASSIGN
              | '(' INTF typedeclname_list ')' typedeclname ASSIGN isalist'''
  if len(p) == 4:
    name = p[2]
    isa = []
    genargs = []
  elif len(p) == 5:
    name = p[2]
    isa = p[4]
    genargs = []
  elif len(p) == 7:
    name = p[5]
    isa = []
    genargs = p[3]
  elif len(p) == 8:
    name = p[5]
    isa = p[7]
    genargs = p[3]
  p[0] = ast.Intf(name, genargs, isa, [], [], [], [])

def p_intfdecl(p):
  '''intfdecl : INTF typedeclname ASSIGN typedecl_block
              | INTF typedeclname ASSIGN isalist typedecl_block
              | '(' INTF typedeclname_list ')' typedeclname ASSIGN typedecl_block
              | '(' INTF typedeclname_list ')' typedeclname ASSIGN isalist typedecl_block'''
  if len(p) == 5:
    name = p[2]
    isa = []
    genargs = []
    block = p[4]
  elif len(p) == 6:
    name = p[2]
    isa = p[4]
    genargs = []
    block = p[5]
  elif len(p) == 8:
    name = p[5]
    isa = []
    genargs = p[3]
    block = p[7]
  elif len(p) == 9:
    name = p[5]
    isa = p[7]
    genargs = p[3]
    block = p[8]

  _, typedecls, decls, methods, funs, semantics = _typedef_block(block)
  p[0] = ast.Intf(name, genargs, isa, typedecls, decls, methods, funs)

def p_modname(p):
  '''modname : IDENT
             | IDENT DOT modname'''
  if len(p) == 2:
    p[0] = [p[1]]
  else:
    p[0] = [p[1]] + p[3]

def p_import(p):
  '''import : IMPORT modname
            | FROM modname IMPORT TIMES
            | FROM modname IMPORT idents'''
  path = p[2]
  if len(p) == 3:
    p[0] = ast.Import(path)
  else:
    if p[4] == '*':
      p[0] = ast.Import(path, all=True)
    else:
      p[0] = [ast.Import(path + [id.name], alias=id.name) for id in p[4]]

def p_declare(p):
  '''declare : DECLARE modname ASSIGN declare_block'''
  p[0] = ast.Declare(p[2], p[4])

def p_toplevel(p):
  '''toplevel : import
              | declare
              | typedecl
              | fundecl
              | methoddecl
              | intfdecl
              | static_pattern'''
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
_define_block('_statements_block', 'statement', 'statement_witheol')
_define_block('typedecl_block', 'typedecl_statement')
_define_block('matchers_block', 'matcher')
_define_block('declare_block', 'toplevel')

def p_error(t):
  if t is None:
    raise errors.ParseError("Unexpected EOF: %s" \
        % (ast.gmodctx[ast.gmodname[-1]].fn))
  else:
    raise errors.ParseError("Syntax error at: %s:%s: '%s'" \
        % (ast.gmodctx[ast.gmodname[-1]].fn, t.lineno, t.value))

start = 'module'

def parsefile(modname, fn):
  with open(fn) as f:
    lex.lex()
    global gfn
    gfn = fn
    yacc.yacc(outputdir='bootstrap')
    content = f.read()
    ast.gmodname.append(modname)
    ast.gmodctx[modname] = ast.ModuleContext(modname, fn)
    mod = ast.Module(yacc.parse(content, debug=0))
    mod.setname(modname, filename=fn)
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
