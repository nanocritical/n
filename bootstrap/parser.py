import re
import copy

import errors
import ast
import resolv

xident = r'([A-Za-z_]\w*)'
xtypename = r'([A-Z]\w*)'
xbinop = r'(==|!=|<=|>=|<|>|\+|-|\*|/|%|&|\||\^|and|or|not)'
xunop = r'(-|~)'

class M(object):
  def __init__(self, s):
    self.s = s
    self.pos = 0
    self.end = len(s)
    self.stack = []
  def cur(self):
    return self.s[self.pos]
  def adv(self, by=1):
    self.pos += by
  def eof(self):
    return self.pos == self.end
  def save(self):
    self.stack.append(copy.copy(self))
  def restore(self):
    saved = self.stack[-1]
    self.s = saved.s
    self.pos = saved.pos
    self.end = saved.end
    self.stack = self.stack[:-1]
  def forget(self):
    self.stack = self.stack[:-1]
  def sub(self, start, end=None):
    m = M(self.s)
    m.pos = start
    m.end = end or self.end
    return m
  def str(self):
    return self.s[self.pos:self.end]


class Pnode(object):
  def __init__(self, what):
    self.what = what
    self.parseto = None
  def __eq__(self, other):
    return self.what == other.what
  def __ne__(self, other):
    return self.what != other.what
  def to(self, p):
    self.parseto = p
    return self
  def parse(self, node):
    if self.parseto is None:
      return node
    else:
      return self.parseto.parse(node)

class OneOf(Pnode):
  def __init__(self, *oneof):
    super(OneOf, self).__init__(oneof)

class Seq(Pnode):
  def __init__(self, *seq):
    super(Seq, self).__init__(seq)

class Repeat(Pnode):
  pass

class Maybe(Pnode):
  pass

class Block(Pnode):
  def __init__(self, *seq):
    super(Block, self).__init__(seq)

class BlockStatement(Pnode):
  pass

class Group(Pnode):
  pass


def _pmatch(m, s):
  pspace(m)
  if m.s[m.pos:].startswith(s):
    m.adv(len(s))
    return s
  else:
    return None

def _normalize(terms):
  if not isinstance(terms, list):
    r = terms
  elif len(terms) == 0:
    r = None
  elif len(terms) == 1:
    r = terms[0]
  else:
    r = terms
  return r

def _poneof(m, oneof):
  pspace(m)
  for p in oneof.what:
    if isinstance(p, Maybe):
      raise Exception("Cannot have a Maybe node here '%s'" % p)
    m.save()
    n = _ptree(m, p)
    if n is not None:
      m.forget()
      return oneof.parse(n)
    m.restore()
  return None

def _pseq(m, seq):
  pspace(m)
  result = []
  m.save()
  for p in seq.what:
    n = _ptree(m, p)
    if n is None:
      if isinstance(p, Maybe):
        continue
      m.restore()
      return None
    else:
      result.append(n)
  m.forget()
  return seq.parse(result)

def _prepeat(m, repeat):
  if isinstance(repeat.what, Maybe):
    raise Exception("Cannot have a Maybe node here '%s'" % p)

  pspace(m)
  result = []
  while True:
    m.save()
    n = _ptree(m, repeat.what)
    if n is None:
      m.restore()
      break
    else:
      m.forget()
      result.append(n)

  if len(result) == 0:
    return None
  else:
    return repeat.parse(result)

def _pmaybe(m, maybe):
  pspace(m)
  return _ptree(m, maybe.what)

gblockindent = 0

def _pblock(m, seq):
  pspace(m)
  if m.cur() != '{' and m.cur() != '\n':
    return None

  style = m.cur()
  m.adv()
  first = True
  global gblockindent
  oldindent = gblockindent
  result = []

  for p in seq.what:
    m.save()
    if style == '\n':
      m.save()
      n = pindentation(m)
      if (first and n <= gblockindent) or n < gblockindent:
        m.restore()
        break
      else:
        m.forget()
      if not first and n > gblockindent:
        raise errors.ParseError("Unexpectedly deep indentation: %s" % m.str())
      gblockindent = n

    elif style == '{':
      if m.cur() == '}':
        break

    r = _ptree(m, p)
    if r is None:
      if isinstance(p, Maybe):
        m.restore()
        if first:
          gblockindent = oldindent
        continue
      else:
        gblockindent = oldindent
        result = []
        break
    else:
      m.forget()
      first = False
      result.append(r)

  gblockindent = oldindent
  if len(result) == 0:
    return None
  else:
    return seq.parse(result)

def _pblockstatement(m, node):
  if isinstance(node, Maybe):
    raise Exception("Cannot have a Maybe node here '%s'" % p)
  m.save()
  r = _ptree(m, node.what)
  if r is None:
    m.restore()
    return None

  if peol(m) is None:
    m.restore()
    return None
  else:
    m.forget()
    return node.parse(r)

def _pgroup(m, node):
  n = pgroup(m)
  if n is None:
    return None
  m.save()
  r = _ptree(n, node.what)
  if r is None:
    m.restore()
    return None
  else:
    m.forget()
    return node.parse(r)


def _ptree(m, node):
  pspace(m)
  if m.eof():
    return None

  if isinstance(node, basestring):
    r = _pmatch(m, node)
  elif isinstance(node, _ptree.__class__):  # A function
    r = node(m)
  elif isinstance(node, OneOf):
    r = _poneof(m, node)
  elif isinstance(node, Seq):
    r = _pseq(m, node)
  elif isinstance(node, Repeat):
    r = _prepeat(m, node)
  elif isinstance(node, Maybe):
    r = _pmaybe(m, node)
  elif isinstance(node, Block):
    r = _pblock(m, node)
  elif isinstance(node, BlockStatement):
    r = _pblockstatement(m, node)
  elif isinstance(node, Group):
    r = _pgroup(m, node)
  else:
    raise Exception("Unknown parse node '%s'" % node)

  return _normalize(r)


def pspace(m):
  escaped = False
  while not m.eof():
    c = m.cur()
    if c != '\n' and c.isspace():
      m.adv()
    elif c == '\\':
      escaped = True
      m.adv()
    elif c == '\n' and escaped:
      escaped = False
      m.adv()
    elif c == '\n':
      break
    else:
      break

def peol(m):
  pspace(m)
  if m.eof():
    return None
  c = m.cur()
  if c == '\n':
    m.adv()
    return '\n'
  elif c == ';':
    m.adv()
    return '{'
  elif c == '{' or c == '}':
    # do not adv
    return '{'
  else:
    return None

def pindentation(m):
  n = 0
  while not m.eof() and m.cur().isspace():
    n += 1
    m.adv()
  return n

def pgroup(m):
  pspace(m)
  if m.cur() != '(':
    return None

  count = 1
  m.adv()
  start, end = m.pos, None
  while not m.eof():
    c = m.cur()
    m.adv()
    if c == '(':
      count += 1
    elif c == ')':
      count -= 1
      if count == 0:
        end = m.pos
        break
  if end is None:
    raise errors.ParseError('Unterminated group')
  return m.sub(start, end)

def pident(m):
  pspace(m)
  rm = re.match(xident, m.str())
  if rm is None:
    return None
  elif rm.group(1) == 'fun' or rm.group(1) == 'type':
    return None
  else:
    m.adv(len(rm.group(0)))
    return rm.group(1)

def pidents(m):
  ids = []
  while True:
    id = pident(m)
    if id is None:
      break
    ids.append(id)
  if len(ids) == 0:
    return None
  else:
    return ids

def plitstr(m):
  pspace(m)
  if m.cur() != "'":
    return None

  start, end = m.pos, None
  while not m.eof():
    c = m.cur()
    if c == '\\':
      m.adv()
    elif c == "'":
      end = m.pos()
      break
    m.adv()
  if end is None:
    raise errors.ParseError('Unterminated string literal')
  return m.sub(start, end).str()

def plitint(m):
  pspace(m)
  if not m.cur().isdigit() and m.cur() != '-':
    return None
  start = m.pos
  m.adv()
  while not m.eof() and m.cur().isdigit():
    m.adv()
  end = m.pos
  return long(m.sub(start, end).str())

def punop(m):
  pspace(m)
  rm = re.match(xunop, m.str())
  if rm is None:
    return None
  else:
    m.adv(len(rm.group(0)))
    return rm.group(1)

def pbinop(m):
  pspace(m)
  rm = re.match(xbinop, m.str())
  if rm is None:
    return None
  else:
    m.adv(len(rm.group(0)))
    return rm.group(1)

def pexprleaf(m):
  return _ptree(m, OneOf(
    OneOf(pident).to(ast.Ident),
    OneOf(plitstr, plitint).to(ast.Literal)))

def pexprterm3(m):
  return _ptree(m, OneOf(
    Seq(Group(pexpr), pbinop, pexpr).to(ast.BinOp),
    Seq(pexprleaf, pbinop, pexpr).to(ast.BinOp),
    Seq(punop, pexpr).to(ast.UnOp),
    Seq(punop, pexpr).to(ast.UnOp),
    pexprleaf))

def pexprterm2(m):
  return _ptree(m, OneOf(
    Seq(Group(pexpr), ',', pexpr).to(ast.Tuple),
    Seq(pexprterm3, ',', pexpr).to(ast.Tuple),
    pexprterm3))

def pexprterm1(m):
  return _ptree(m, OneOf(
    Seq(Group(pexpr), ':', ptype).to(ast.ConstrainedExpr),
    Seq(pexprterm2, ':', ptype).to(ast.ConstrainedExpr),
    pexprterm2))

def pexpr(m):
  return _ptree(m, OneOf(
    Seq(pexprterm1, Repeat(pexpr)).to(ast.Call),
    Group(pexpr),
    pexprterm1))

def ptypename(m):
  pspace(m)
  rm = re.match(xtypename, m.str())
  if rm is None:
    return None
  else:
    m.adv(len(rm.group(0)))
    return ast.Type.parse(rm.group(0))

class _Typeapp:
  @classmethod
  def parse(cls, args):
    assert len(args) == 2
    if isinstance(args[1], list):
      return map(ast.Type.parse, [args[0]] + args[1])
    else:
      return map(Type.parse, args)

def _ptypetuple(m):
  m.save()
  t0 = ptypename(m) or ptype(m, True)
  if t0 is None:
    return None
  r = [t0]
  while not m.eof():
    t = _pmatch(m, ',');
    if t is None:
      break
    t = ptypename(m) or ptype(m, True)
    if t is None:
      break
    r.append(t)

  if len(r) == 1:
    m.restore()
    return None
  else:
    m.forget()
    return ast.TypeTuple.parse(tuple(r))

def _ptypeslice(m):
  return _ptree(m, Seq('[]', OneOf('!', '.'), ptype).to(ast.TypeSlice))

def _ptypeapp_parse(m):
  m.save()
  t0 = ptypename(m)
  if t0 is None:
    return None
  r = [t0]
  while not m.eof():
    pspace(m)
    if m.eof():
      break
    t = ptypename(m) or ptype(m, True)
    if t is None:
      break
    r.append(t)

  if len(r) == 1:
    m.restore()
    return None
  else:
    m.forget()
    return r

def _ptypeapp(m):
  r = _ptypeapp_parse(m)
  if r is None:
    return None
  else:
    ast.TypeApp.parse(r)

def _ptypegeneric(m):
  r = _ptypeapp_parse(m)
  if r is None:
    return None
  else:
    return ast.TypeGeneric.parse(r)

def ptype(m, needgroup=False):
  pattern = OneOf(
      Seq(OneOf('.', '!'),
        OneOf(Group(_ptypetuple), Group(_ptypeapp), ptypename)).to(ast.TypeRef),
      OneOf(_ptypeslice, _ptypetuple, _ptypeapp, ptypename))
  if needgroup:
    return _ptree(m, Group(pattern))
  else:
    return _ptree(m, pattern)

def ptypedeclname(m):
  return _ptree(m, OneOf(_ptypegeneric, ptypename))

def ptypedident(m):
  return _ptree(m, Seq(pidents, ':', ptype).to(ast.VarDecl))

def pvardecl(m):
  return _ptree(m,
      BlockStatement(OneOf(
        Seq('let', ptypedident, Maybe(Seq('=', pexpr))).to(ast.VarDecl),
        Seq('let', pident, '=', pexpr).to(ast.VarDecl))))

def pfielddecl(m):
  return _ptree(m, BlockStatement(Seq(pidents, ':', ptype).to(ast.FieldDecl)))

def passign(m):
  return _ptree(m, BlockStatement(Seq(pident, '=', pexpr).to(ast.Assign)))

def preturn(m):
  return _ptree(m, BlockStatement(Seq('return', pexpr).to(ast.Return)))

def passert(m):
  return _ptree(m, BlockStatement(Seq('?', pexpr).to(ast.Assert)))

def psemanticstatement(m):
  pos = m.pos
  while m.cur() != '\n':
    m.adv()
  return m.s[pos:m.pos]

def psemanticassert(m):
  return _ptree(m, BlockStatement(Seq('!', psemanticstatement).to(ast.SemanticAssert)))

def psemanticclaim(m):
  return _ptree(m, BlockStatement(Seq('~', psemanticstatement).to(ast.SemanticClaim)))

def pstatement(m):
  return _ptree(m, OneOf(preturn, passert, pvardecl, passign,
    psemanticassert, psemanticclaim, BlockStatement(pexpr)))

def pchoicedecl(m):
  return _ptree(m,
      OneOf(
        BlockStatement(
          Seq('|', OneOf(pident, passign)).to(ast.ChoiceDecl)),
        BlockStatement(
          Seq('|', OneOf(pident, passign), '->', ptype).to(ast.ChoiceDecl))))

def ptypedecl(m):
  return _ptree(m,
      Seq('type', ptypedeclname, '=',
        Maybe(Block(
          Maybe(Repeat(pimport)),
          Maybe(Repeat(ptypedecl)),
          Maybe(OneOf(
            Repeat(pchoicedecl),
            Repeat(pfielddecl))),
          Maybe(Repeat(OneOf(pfundecl, pmethoddecl)))))).to(ast.TypeDecl))

def pfundecl(m):
  return _ptree(m,
      Seq('fun', pident, Maybe(Repeat(ptypedident)), '=',
        Repeat(OneOf(ptypedident, ptype)),
        Maybe(Block(Repeat(pstatement)))).to(ast.Function))

def pmethoddecl(m):
  return _ptree(m,
      Seq('method', Maybe('!'), pident, Maybe(Repeat(ptypedident)), '=',
        Repeat(OneOf(ptypedident, ptype)),
        Maybe(Block(Repeat(pstatement)))).to(ast.Method))

def pintfdecl(m):
  return _ptree(m,
      Seq(OneOf('intf', 'dynintf'), ptypedeclname, '=',
        Maybe(Block(Repeat(OneOf(pfundecl, pmethoddecl))))))

def ptoplevel(m):
  return _ptree(m, OneOf(ptypedecl, pfundecl, pmethoddecl, Seq(pvardecl, peol)))

class _ModName(object):
  @classmethod
  def parse(cls, args):
    return args[0] + ''.join(args[1])

def pmodname(m):
  return _ptree(m, OneOf(
    Seq(pident, Repeat(Seq('.', pident))),
    pident).to(_ModName))

def pimport(m):
  return _ptree(m,
      OneOf(
        Seq('import', pidents, peol),
        Seq('from', pmodname, 'import', '*', peol),
        Seq('from', pmodname, 'import', pidents, peol)).to(ast.Import))

def pmodule(m):
  return _ptree(m,
    Maybe(Repeat(OneOf(pimport, ptoplevel, peol)).to(ast.Module)))

def removecomments(m):
  return M(re.sub(r'[^\\]?--.*$', '', m.str(), re.S))

def parse(m):
  m = removecomments(m)
  mod = pmodule(m)
  return mod, m

def parsefile(modname, fn):
  with open(fn) as f:
    m = M(f.read())
    ast.gmodname = modname
    ast.gmodctx = ast.ModuleContext()
    ast.gctx[modname] = ast.GlobalContext()
    mod, m = parse(m)
    if m.str() != '':
      raise errors.ParseError("Rejected content:\n" + m.str())
    mod.name = modname
    mod.ctx = ast.gmodctx
    ast.gmodctx = None
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
