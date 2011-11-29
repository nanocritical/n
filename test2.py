from bootstrap.parser import yacc
from bootstrap import ast

ast.gmodname = 'test'
ast.gmodctx = ast.ModuleContext()
ast.gctx['test'] = ast.GlobalContext()
s = '''fun foo x:U32 = U32
  let y:U32 = 0
  return
'''
print yacc.parse(s)
