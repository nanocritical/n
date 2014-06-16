if exists("b:current_syntax") | finish | endif
let b:current_syntax = "n"

syn keyword nInclude import from
syn keyword nDecl union fun met shallow intf delegate struct enum atom
syn keyword nStorageClass inline extern opaque
syn keyword nExport export
syn keyword nSemantic unique shared pshared
syn keyword nSemantic capturedby capturedbyret owned ownedby protected protect protecting prop claim logic

syn keyword nAssert Assert pre post invariant Pre Post Invariant
syn keyword nDecl example alias within globalenv
syn keyword nDecl contract honors _honors

syn keyword nConditional if elif else match
syn keyword nRepeat while for pfor foreach pforeach
syn keyword nKeyword let block lambda async such
syn keyword nStatement return continue break unreached
syn keyword nOperator in and or not isa
syn match nDecl "^  isa" contains=nExport
syn keyword nOperator sizeof alignof
syn keyword nKeyword noop as attr declare
syn keyword nKeyword try catch
syn keyword nException except throw drop fatal
syn match nOperator "bw[&|\^~]"
syn match nOperator "[@&\:+\-\*/%]"
syn match nOperator "[\:+\-\*/%]\?="
syn match nOperator "\(<<\|>>\|<=\|>=\|<\|>\|===\|!==\|==\|!=\)"

syn match nSemantic "::"

syn keyword nKeyword self this final

syn keyword Constant null false true

syn region nComment start="--" skip="\\$" end="$" keepend contains=@Spell,nTodo
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
