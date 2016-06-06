if exists("b:did_indent") | finish | endif
let b:did_indent = 1

setlocal ts=8
setlocal sw=8
setlocal sts=0
setlocal noexpandtab

setlocal nolisp
setlocal autoindent

setlocal indentexpr=GetNlangIndent(v:lnum)
setlocal indentkeys="o,O,0{,0},!^F,<\\>,0=elif\ ,0=else,0=catch,0=struct\ ,0=intf\ ,0=fun\ ,=met\ ,0\|"

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

  let TOP = '^\(export\s\+\)\?\(extern\s\+\)\?\(inline\s\+\)\?'
  let FUN = TOP . '(\?fun'
  let STRUCT_INTF = TOP . '\(struct\|intf\)'
  let INTF_FUN_MET = '^\s\+\(fun\|met\)'
  let STRUCT_FUN_MET = TOP . '\w\+\s\+(\?\(fun\|met\)'

  let in_intf = 0
  let in_struct = 0
  let pzero = a:lnum - 1
  let pone = - 1
  while pzero >= 0
    let l = getline(pzero)
    if l =~ '^\t\S'
      let pone = pzero
    endif
    if l =~ '^\S'
      break
    endif
    let pzero = pzero - 1
  endwhile

  let pzeroline = getline(pzero)
  if pzeroline =~ '\<intf\>'
    let in_intf = 1
  elseif pzeroline =~ '\<struct\>'
    let in_struct = 1
  endif

  let line = getline(a:lnum)
  if in_intf || in_struct
    return &sw
  elseif line =~ STRUCT_INTF
    return 0
  elseif line =~ STRUCT_FUN_MET || line =~ FUN
    return 0
  endif

  " It's generally dangerous to try to automatically indent this kind of block,
  " as we cannot be sure what was intended (as the intentation *is* the intent).
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
  if pline =~ FUN  || pline =~ STRUCT_INTF || pline =~ STRUCT_FUN_MET
    return indent(prevl) + &sw
  elseif pline =~ '^\s*\<\(if\|elif\|else\|for\|while\|try\|catch\|assert\|pre\|post\|invariant\)\>'
    return indent(prevl) + &sw
  elseif pline =~ '\<\(block\|such\)\>$'
    return indent(prevl) + &sw
  elseif pline =~ '^\s*|'
    return indent(prevl) + &sw
  endif

  return indent(pline)
endfunction
