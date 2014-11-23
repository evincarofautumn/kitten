" Vim syntax file
" Language: Kitten
" Maintainer: Jon Purdy <evincarofautumn@gmail.com>

syn case match

syn keyword kittenBool false true
syn keyword kittenBuiltinType bool char float handle int
syn keyword kittenKeyword abbrev case choice data define default else if infix infix import match option vocab
syn keyword kittenTodo FIXME HACK NOTE TODO XXX

syn match kittenEscape /\\["&'\\abfnrtv]/ contained
syn match kittenIdent /[a-z_][0-9A-Za-z_]*/
syn match kittenCharacter /[^0-9A-Za-z_']'\([^\\]\|\\\([^']\+\|'\)\)'/lc=1 contains=kittenEscape
syn match kittenCharacter /^'\([^\\]\|\\\([^']\+\|'\)\)'/ contains=kittenEscape
syn match kittenDelimiter "(\|)\|,\|:\|;\|\[\|\]\|{\|}"
syn match kittenIntrinsic /__[0-9a-z_]\+/
syn match kittenSymbol /[!#$%&\*+,-./<=>?@^|~]\+/
syn match kittenType /[A-Z][0-9A-Za-z_]*/

syn match kittenInt "\<[0-9]\+\>\|\<0x[0-9a-fA-F]\+\>\|\<0o[0-7]\+\>\|\<0b[01]\+\>"
syn match kittenFloat "\<[0-9]\+\.[0-9]\+\>"

syn region kittenComment start="//" end="$" contains=kittenTodo
syn region kittenMultiComment start="/\*" end="\*/" contains=kittenTodo,kittenMultiComment
syn region kittenString start='"' skip='\\\\\|\\"' end='"' contains=kittenEscape

let b:current_syntax = "kitten"

hi def link kittenBool Boolean
hi def link kittenBuiltinType Type
hi def link kittenCharacter Character
hi def link kittenComment Comment
hi def link kittenDelimiter Normal
hi def link kittenEscape SpecialChar
hi def link kittenFloat Float
hi def link kittenIdent Identifier
hi def link kittenInt Number
hi def link kittenIntrinsic Keyword
hi def link kittenKeyword Keyword
hi def link kittenMultiComment Comment
hi def link kittenString String
hi def link kittenSymbol Normal
hi def link kittenTodo Todo
hi def link kittenType Type
