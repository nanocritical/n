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
setlocal indentkeys=o,O,0{,0},!^F,<\\>,0=elif\ ,0=else,0=catch,0=type\ ,0=intf\ ,0=fun\ ,0=delegate\ ,=method\ ,0\|,*<Return>

if exists("*GetNlangIndent")
  finish
endif

function NlangPrevnonblank(lnum)
  let p = prevnonblank(a:lnum)
  while getline(p) =~ '^\s*--'
    let p = prevnonblank(p - 1)
  endwhile
  return p
endfunction

function GetNlangIndent(lnum)
  let prevl = NlangPrevnonblank(a:lnum - 1)
  if prevl == 0
    return 0
  endif
  let pprevl = NlangPrevnonblank(prevl - 1)

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
  let pone = - 1
  while pzero >= 0
    let l = getline(pzero)
    if l =~ '^  \S'
      let pone = l
    endif
    if l =~ '^\S'
      if l =~ '^intf'
        let in_intf = 1
      elseif l =~ '^type'
        let in_type = 1
      elseif l =~ '^fun'
        let in_fun = 1
      elseif l =~ '^\s*\w\+\s\+\<method\>'
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
  elseif line =~ '^\s*\<\(type\|fun\|intf\|contract\)\>'
    return 0
  elseif line =~ '^\s*\w\+\s\+\<\(fun\|method\)\>'
    return 0
  elseif line =~ '^\s*\<\(delegate\)\>'
    return &sw
  endif


  " It's generally dangerous to try to automatically indent this kind of block,
  " as we cannot be sure what was intented (as the intentation *is* the intent).
  " However, when only one series of if/elif/else is present, or a single
  " try/catch, we assume it is safe to do the obvious. Similarly, when an
  " elif/else or catch is further to the right than any if or catch, even in the
  " presence of nesting, we assume it's OK to bring it back to the left, to the
  " outermost indent. Same idea with match and |.
  if line =~ '^\s*\<\(elif\|else\)\>'
    let ambiguous = 0
    let outermost = 0
    let p = a:lnum
    while p >= pone
      if getline(p) =~ '^\s*\<\(if\)\>'
        let ambiguous = ambiguous + 1
        if indent(p) > outermost
          let outermost = indent(p)
        endif
      endif
      let p = p - 1
    endwhile
    if getline(prevl) =~ '^\s*if.*->' || getline(prevl) =~ '^\s*elif.*->'
      return indent(prevl)
    elseif ambiguous == 1 || indent(a:lnum) > outermost
      return indent(prevl) - &sw
    else
      return indent(a:lnum)
    endif
  elseif line =~ '^\s*\<catch\>'
    let ambiguous = 0
    let outermost = 0
    let p = a:lnum
    while p >= pone
      if getline(p) =~ '^\s*\<\(try\)\>'
        let ambiguous = ambiguous + 1
        if indent(p) > outermost
          let outermost = indent(p)
        endif
      endif
      let p = p - 1
    endwhile
    if getline(prevl) =~ '^\s*try\s*->'
      return indent(prevl)
    elseif ambiguous == 1 || indent(a:lnum) > outermost
      return indent(prevl) - &sw
    else
      return indent(a:lnum)
    endif
  elseif line =~ '^\s*|'
    let ambiguous = 0
    let outermost = 0
    let p = a:lnum
    while p >= pone
      if getline(p) =~ '^\s*\<match\>'
        let ambiguous = ambiguous + 1
        if indent(p) > outermost
          let outermost = indent(p)
        endif
      endif
      let p = p - 1
    endwhile
    if getline(prevl) =~ '^\s*\<match\>' || getline(prevl) =~ '^\s*|.*->'
      return indent(prevl)
    elseif ambiguous == 1 || indent(a:lnum) > outermost
      return indent(prevl) - &sw
    else
      return indent(a:lnum)
    endif
  endif

  let pline = getline(prevl)
  if pline =~ '^\s*\<fun\>' || pline =~ '^\s*\w\+\s\+\<\(fun\|method\)\>'
    return indent(prevl) + &sw
  elseif pline =~ '^\s*\<\(if\|elif\|else\|for\|while\|try\|catch\|intf\|type\)\>'
    return indent(prevl) + &sw
  elseif pline =~ '^\s*|'
    return indent(prevl) + &sw
  endif

  return indent(pline)
endfunction
