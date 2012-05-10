if exists("b:current_syntax")
  finish
endif

syn keyword nDecl type fun method union intf dynintf import inherit
syn match nDecl "^\s*from"

syn keyword nKeyword let if elif else for while continue break extern
syn keyword nKeyword match except return block future pfor
syn keyword nKeyword in and or not neg isa
syn keyword nKeyword sizeof pass as attr declare except try catch throw
syn match nKeyword "[@:]"

syn keyword Constant null false true

syn region nComment start="--" skip="\\$" end="$" keepend contains=@Spell,nTodo
syn keyword nTodo contained TODO FIXME XXX

syn region nSemantic start="^\s*#[!~?]" end="$" keepend
syn match nMutate "[!][^=]"me=e-1
syn match nNullable "?"

syn match nSpaceError display excludenl "\s\+$"
syn match nSpaceError display " \+\t"me=e-1

"integer number, or floating point number without a dot and with "f".
syn case ignore
syn match	nNumbers	display transparent "\<\d\|\.\d" contains=nNumber,nFloat,nOctalError,nOctal
" Same, but without octal error (for comments)
syn match	nNumbersCom	display contained transparent "\<\d\|\.\d" contains=nNumber,nFloat,nOctal
syn match	nNumber		display contained "\d\+\(u\=l\{0,2}\|ll\=u\)\>"
"hex number
syn match	nNumber		display contained "0x\x\+\(u\=l\{0,2}\|ll\=u\)\>"
" Flag the first zero of an octal number as something special
syn match	nOctal		display contained "0\o\+\(u\=l\{0,2}\|ll\=u\)\>" contains=nOctalZero
syn match	nOctalZero	display contained "\<0"
syn match	nFloat		display contained "\d\+f"
"floating point number, with dot, optional exponent
syn match	nFloat		display contained "\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\="
"floating point number, starting with a dot, optional exponent
syn match	nFloat		display contained "\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
"floating point number, without dot, with exponent
syn match	nFloat		display contained "\d\+e[-+]\=\d\+[fl]\=\>"
"hexadecimal floating point number, optional leading digits, with dot, with exponent
syn match	nFloat		display contained "0x\x*\.\x\+p[-+]\=\d\+[fl]\=\>"
"hexadecimal floating point number, with leading digits, optional dot, with exponent
syn match	nFloat		display contained "0x\x\+\.\=p[-+]\=\d\+[fl]\=\>"

" flag an octal number with wrong digits
syn match	nOctalError	display contained "0\o*[89]\d*"
syn case match

hi def link nDecl Structure
hi def link nKeyword Conditional
hi def link nComment Comment
hi def link nTodo Todo
hi def link nSpaceError Error
hi def link nNumber Number
hi def link nFloat Number
hi def link nOctal Number
hi def link nNumbers Number
hi def link nString String
hi def link nSemantic Semantic
hi def link nMutate Constant
hi def link nNullable Constant

let b:current_syntax = "n"
