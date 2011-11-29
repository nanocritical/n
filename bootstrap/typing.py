import ast
import errors

def _unifyerror(*types):
  raise errors.TypeError("Cannot unify the types %s" % map(str, types))

def _unify_literals(lits):
  if len(lits) == 0:
    return None

  if len(set(lits)) > 1:
    _unifyerror(*lits)
  return lits[0]

def _unify_lit_conc(lit, conc):
  if lit.name == 'nlang.literal.Integer':
    if conc.name in \
        ['U8', 'U16', 'U32', 'U64',
            'I8', 'I16', 'I32', 'I64',
            'Size', 'SSize']:
      return conc
  elif lit.name == 'nlang.literal.String':
    if conc.name == 'String' or conc.name == 'Char':
      return conc
  elif lit.name == 'nlang.literal.Null':
    if isinstance(conc, ast.TypeRefNullable):
      return conc

  _unifyerror(lit, conc)

def _isliteral(type):
  return type.name.startswith('nlang.literal.')

def _isconcrete(type):
  return not type.name.startswith('nlang.literal.')

def unify(types):
  assert len(types) > 0

  ulit = _unify_literals(filter(_isliteral, types))

  concretes = filter(_isconcrete, types)
  if len(set(concretes)) > 1:
    _unifyerror(*concretes)

  if ulit is not None and len(concretes) > 0:
    return _unify_lit_conc(ulit, concretes[0])
  elif ulit is not None:
    return ulit
  elif len(concretes) > 0:
    return concretes[0]

def checkcompat(target, type):
  u = unify([target, type])
  if target != u:
    _unifyerror(target, type)
