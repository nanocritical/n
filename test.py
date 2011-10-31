import sys
import StringIO

from bootstrap.ast import *
from bootstrap.parser import *
import bootstrap.cwriter
import bootstrap.resolv as resolv

def TEQ(x, y):
  if x != y:
    raise Exception("Test error: %r != %r" % (x, y))

def TNEQ(x, y):
  if x != y:
    raise Exception("Test error: %r != %r" % (x, y))


m = M('123')
TEQ(123L, plitint(m))
m = M('123')
TEQ(Expr(123L), pexprterm(m))
m = M('123')
TEQ(Expr(123L), pexpr(m))

m = M('-123')
TEQ(-123L, plitint(m))
m = M('-123')
TEQ(Expr(-123L), pexprterm(m))

m = M('123 + 23')
TEQ(Expr(Expr(123L), Operator('+'), Expr(23L)), pexpr(m))
m = M('42 * (123 + 23)')
TEQ(Expr(Expr(42L), Operator('*'), Expr(Expr(123L), Operator('+'), Expr(23L))), pexpr(m))
m = M('(calling a fun)')
TEQ(Expr(Expr('calling'), Expr('a'), Expr('fun')), pexpr(m))
m = M('calling a fun')
TEQ(Expr(Expr('calling'), Expr('a'), Expr('fun')), pexpr(m))

Void = Type('Void')
U32 = Type('U32')
I32 = Type('I32')
U16 = Type('U16')

m = M('x :U32\n')
TEQ(VarDecl('x', U32), pvardecl(m))
m = M('x :U32 := 12\n')
TEQ(VarDecl('x', U32, Expr(12L)), pvardecl(m))
m = M('x = 12')
TEQ(None, passign(m))  # Missing EOL
m = M('x = (12)\n')
TEQ(Assign('x', Expr(12L)), passign(m))

m = M('x = 12\n')
TEQ(Assign('x', Expr(12L)), pstatement(m))

s = StringIO.StringIO()
Expr('x', Operator('+'), 'y').cwrite(s)
TEQ('x   +   y  ', s.getvalue())

s = StringIO.StringIO()
Expr(pexpr(M('x + y'))).cwrite(s)
TEQ('x   +   y  ', s.getvalue())

TEQ(Type('A'), ptype(M('A')))
TEQ(TypeTuple(Type('A'), Type('B')), ptype(M('A, B')))
TEQ(TypeApp(Type('A'), Type('T'), Type('U')), ptype(M('A T U')))
TEQ(None, ptype(M('(A T U)')))
TEQ(TypeApp(Type('A'), TypeApp(Type('T'), Type('U'))), ptype(M('A (T U)')))
TEQ(TypeTuple(TypeApp(Type('T'), Type('U')), Type('B')), ptype(M('(T U), B')))

dx = VarDecl('x', U32)
dx0 = VarDecl('x', I32, Expr(0L))
dy = VarDecl('y', U16)

TEQ(TypeDecl(Type('A'), [], [], [], [dx], [], []),
    ptypedecl(M('type A =\n  x :U32\n')))
TEQ(TypeDecl(Type('A'), [], [], [], [dx], [], []),
    ptypedecl(M('type A = {  x :U32 }')))
TEQ(TypeDecl(Type('A'), [], [], [], [dx, dy], [], []),
    ptypedecl(M(
'''type A =
  x :U32
  y:U16
''')))
TEQ(TypeDecl(Type('A'), [], [], [], [dx, dy], [], []),
    ptypedecl(M('type A = {  x :U32; y:U16; }')))

foo = Function('foo', [], [Void], [dx0])
bar = Method('bar', [], [Void], [])

TEQ('t/a.n', resolv.find('t.a'))
A = TypeDecl(Type('A'), [], [], [], [VarDecl('x', Type('U32'))], [bar], [foo])
mod_t_a = Module([[A, '\n']])
mod_t_a.name = 't.a'
TEQ(mod_t_a, parsefile('t.a', resolv.find('t.a')))

s = StringIO.StringIO()
foo.cwrite(s)
print s.getvalue()
