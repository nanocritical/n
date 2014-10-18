if exists("b:current_syntax") | finish | endif
let b:current_syntax = "n"

syn keyword nInclude import from
syn keyword nDecl union struct enum atom intf newtype
syn keyword nFunction fun met shallow delegate
syn keyword nStorageClass inline extern opaque
syn keyword nExport export
syn keyword nSemantic unique shared pshared
syn keyword nSemantic capturedby capturedbyret owned ownedby protected protect protecting prop claim logic

syn keyword nAssert assert pre post invariant
syn keyword nDecl example alias within globalenv
syn keyword nDecl contract honors _honors

syn keyword nConditional if elif else match try catch
syn keyword nRepeat while for pfor foreach pforeach
syn keyword nKeyword var let block lambda async such
syn keyword nStatement return continue break unreached
syn keyword nConditional and or
syn match nDecl "^  isa" contains=nExport
syn keyword nOperator sizeof alignof nullable isa Dyncast not in
syn keyword nKeyword noop as declare
syn keyword nException except throw Drop Fatal
syn match nOperator "bw[&|\^~]"
syn match nOperator "ov+"
syn match nOperator "\(ov<<\|>>\|<=\|>=\|<\|>\|===\|!==\|==|\|!=|\|==\|!=\||\)"
syn match nOperator "\(@\|&\|\(ov\)\?[\:+\-\*/%]\)"
syn match nOperator "\(ov\)\?[\:+\-\*/%]\?="

syn match nSemantic "::"

syn keyword nKeyword self this final

syn keyword Constant nil false true

syn region nComment start="--" skip="\\$" end="$" keepend contains=@Spell,nTodo
syn region nComment start="--(" end=")--" keepend contains=@Spell,nTodo
syn keyword nTodo contained TODO FIXME XXX

syn region nSemantic start="^\s*#[!~?]" end="$" keepend
syn match nMutate "[!]$"
syn match nMutate "[!][^=]"me=e-1
syn match nMercurial "#"
syn match nNullable "?"
syn match nWildcard "\$"

syn match nIntf "`\w\+"
set iskeyword+=`

syn match nSpaceError display excludenl "\s\+$"
syn match nSpaceError display "^\t*\ \+"me=e-1

"integer number, or floating point number without a dot and with "f".
syn case ignore
syn match	nNumbers	display transparent "\<\d\|\.\d" contains=nNumber,nFloat,nOctalError,nOctal
" Same, but without octal error (for comments)
syn match	nNumbersCom	display contained transparent "\<\d\|\.\d" contains=nNumber,nFloat,nOctal
syn match	nNumber		display contained "\d[[:digit:]_]*\(u\=l\{0,2}\|ll\=u\)\>"
"hex number
syn match	nNumber		display contained "0x\x[[:xdigit:]_]*\(u\=l\{0,2}\|ll\=u\)\>"
" Flag the first zero of an octal number as something special
syn match	nOctal		display contained "0\o[\o_]*\(u\=l\{0,2}\|ll\=u\)\>" contains=nOctalZero
syn match	nOctalZero	display contained "\<0"
syn match	nFloat		display contained "\d[[:digit:]_]*f"
"floating point number, with dot, optional exponent
syn match	nFloat		display contained "\d[[:digit:]_]*\.[[:digit:]_]*\(e[-+]\=\d[[:digit:]_]*\)\=[fl]\="
"floating point number, starting with a dot, optional exponent
syn match	nFloat		display contained "\.\d[[:digit:]_]*\(e[-+]\=\d[[:digit:]_]*\)\=[fl]\=\>"
"floating point number, without dot, with exponent
syn match	nFloat		display contained "\d[[:digit:]_]*e[-+]\=\d[[:digit:]_]*[fl]\=\>"
"hexadecimal floating point number, optional leading digits, with dot, with exponent
syn match	nFloat		display contained "0x\x[[:xdigit:]_]*\.\x[\x_]*p[-+]\=\d[[:digit:]_]*[fl]\=\>"
"hexadecimal floating point number, with leading digits, optional dot, with exponent
syn match	nFloat		display contained "0x\x[[:xdigit:]_]*\.\=p[-+]\=\d[[:digit:]_]*[fl]\=\>"

" flag an octal number with wrong digits
syn match	nOctalError	display contained "0[\o_]*[89][_\d*]"
syn case match

syn match	nSpecial	display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
syn match	nSpecial	display contained "\\\(u\x\{4}\|U\x\{8}\)"
syn match	nFormat		display "%\(\d\+\$\)\=[-+' #0*]*\(\d*\|\*\|\*\d\+\$\)\(\.\(\d*\|\*\|\*\d\+\$\)\)\=\([hlLjzt]\|ll\|hh\)\=\([aAbdiuoxXDOUfFeEgGcCsSpn]\|\[\^\=.[^]]*\]\)" contained
syn match	nFormat		display "%%" contained
syn region	nString		start=+L\="+ skip=+\\\\\|\\"+ end=+"+ contains=nSpecial,nFormat,@Spell
syn region	nString		start=+L\='+ skip=+\\\\\|\\'+ end=+'+ contains=nSpecial,nFormat,@Spell

hi def link nDecl Structure
hi def link nAssert Assert
hi def link nInclude Include
hi def link nStorageClass StorageClass
hi def link nConditional Conditional
hi def link nRepeat Repeat
hi def link nStatement Statement
hi def link nOperator Operator
hi def link nException Exception
hi def link nKeyword Keyword
hi def link nComment Comment
hi def link nTodo Todo
hi def link nSpaceError Error
hi def link nNumber Number
hi def link nFloat Number
hi def link nOctal Number
hi def link nNumbers Number
hi def link nString String
hi def link nSpecial SpecialChar
"hi def link nSemantic Semantic
"hi def link nMutate Operator
"hi def link nMercurial Operator
hi def link nNullable Constness
hi def link nWildcard Special
hi def link nIntf Type
hi def link nFunction Function
hi def link nExport Export
