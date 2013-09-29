if exists("b:did_indent") | finish | endif
let b:did_indent = 1

setlocal sw=2
setlocal sts=2
setlocal ts=8
setlocal noexpandtab

setlocal nolisp
setlocal autoindent

setlocal indentexpr=GetNlangIndent(v:lnum)
setlocal indentkeys=o,O,0{,0},!^F,<\\>,0=elif\ ,0=else,0=catch,0=type\ ,0=intf\ ,0=fun\ ,0=delegate\ ,=method\ ,0\|,*<Return>

if !exists("no_plugin_maps") && !exists("no_nlang_maps")
  imap <buffer> <expr> <tab> NlangTabComplete()
endif

if exists("*GetNlangIndent")
  finish
endif

function NlangTabComplete()
  let line = getline('.')
  let pre = strpart(line, -1, col('.'))
  if match(pre, '^\s*$') != -1
    return "  "
  else
    return "\<C-V>\<Tab>"
  endif
endfunction

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
    else
      return indent(prevl) + 2 * &sw
    endif
  endif

  let TOP = '^\(export\s\+\)\?\(extern\s\+\)\?\(inline\s\+\)\?'
  let FUN = TOP . '(\?fun'
  let TYPE_INTF = TOP . '\(type\|intf\)'
  let INTF_FUNMETH = '^\s\+\(fun\|method\)'
  let TYPE_FUNMETH = TOP . '\w\+\s\+(\?\(fun\|method\)'

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
      break
    endif
    let pzero = pzero - 1
  endwhile

  let pzeroline = getline(pzero)
  if pzeroline =~ '\<intf\>'
    let in_intf = 1
  elseif pzeroline =~ '\<type\>'
    let in_type = 1
  elseif pzeroline =~ '\<\(fun\|method\)\>'
    let in_fun = 1
  endif

  let line = getline(a:lnum)
  if in_intf && line =~ INTF_FUN_METH
    return &sw
  elseif line =~ TYPE_INTF
    return 0
  elseif line =~ TYPE_FUNMETH
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
  if pline =~ FUN  || pline =~ TYPE_INTF || pline =~ TYPE_FUNMETH
    return indent(prevl) + &sw
  elseif pline =~ '^\s*\<\(if\|elif\|else\|for\|while\|try\|catch\)\>'
    return indent(prevl) + &sw
  elseif pline =~ '^\s*\<\(assert\|pre\|post\|invariant\|block\|such\)\>$'
    return indent(prevl) + &sw
  elseif pline =~ '^\s*|'
    return indent(prevl) + &sw
  endif

  return indent(pline)
endfunction
