if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal sw=2
setlocal sts=2
setlocal expandtab

setlocal nolisp
setlocal autoindent

setlocal indentexpr=GetNlangIndent(v:lnum)
setlocal indentkeys=o,O,0{,0},!^F,<\\>,0=elif,0=else,0=catch,0=type,0=intf,0=fun,0=delegate,=method,0<\|>,*<Return>

if exists("*GetNlangIndent")
  finish
endif

function GetNlangIndent(lnum)
  let prevl = prevnonblank(a:lnum - 1)
  if prevl == 0
    return 0
  endif
  let pprevl = prevnonblank(prevl - 1)

  if getline(prevl) =~ '\\$'
    if prevl > 1 && getline(pprevl) =~ '\\$'
      return indent(prevl)
    endif
    return indent(prevl) + 2 * &sw
  endif

  let in_intf = 0
  let in_type = 0
  let in_fun = 0
  let pzero = a:lnum - 1
  while pzero >= 0
    let l = getline(pzero)
    if l =~ '^\S'
      if l =~ '^intf'
        let in_intf = 1
      endif
      if l =~ '^type'
        let in_type = 1
      endif
      if l =~ '^fun'
        let in_fun = 1
      endif
      if l =~ '^\s*\w\+\s\+\<method\>'
        let in_method = 1
      endif
      break
    endif
    let pzero = pzero - 1
  endwhile

  if in_intf
    return &sw
  endif

  let line = getline(a:lnum)
  if in_intf && line =~ '^\s*\<\(fun\|method\)\>'
    return &sw
  endif
  if line =~ '^\s*\<\(type\|fun\|intf\|contract\)\>'
    return 0
  endif
  if line =~ '^\s*\w\+\s\+\<\(fun\|method\)\>'
    return 0
  endif
  if line =~ '^\s*\<\(delegate\)\>'
    return &sw
  endif
  if line =~ '^\s*\<\(elif\|else\|catch\)\>'
    return indent(prevl) - &sw
  endif

  let pline = getline(prevl)
  if pline =~ '^\s*\<fun\>' || pline =~ '^\s*\w\+\s\+\<\(fun\|method\)\>'
    return indent(prevl) + &sw
  endif
  if pline =~ '^\s*\<\(if\|elif\|else\|for\|while\|try\|catch\|match\|intf\|type\)\>'
    return indent(prevl) + &sw
  endif

  return indent(pline)
endfunction
