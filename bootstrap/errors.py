class Error(Exception):
  pass

class ParseError(Error):
  pass

class ScopeError(ParseError):
  def __init__(self, scope, msg):
    super(ScopeError, self).__init__(str(scope) + ': ' + msg)

class TypeError(ParseError):
  pass

class PmTypeError(ParseError):
  def __init__(self, pattern, type, other):
    super(PmTypeError, self).__init__(
        "Pattern '%s' is matched with incompatible types '%s' and '%s', at:\n%s\n%s" \
        % (pattern, self, other, type.codeloc, other.codeloc))

class PmStructError(ParseError):
  def __init__(self, type, other):
    super(PmStructError, self).__init__(
        "Type '%s' cannot be matched to type '%s', at:\n%s\n%s" \
        % (self, other, type.codeloc, other.codeloc))
