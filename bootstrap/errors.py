class Error(Exception):
  pass

class ParseError(Error):
  pass

class ScopeError(ParseError):
  pass

class TypeError(ParseError):
  pass
