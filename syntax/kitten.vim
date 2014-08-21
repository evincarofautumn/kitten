" Vim syntax file
" Language: Kitten
" Maintainer: Jon Purdy <evincarofautumn@gmail.com>

syntax clear

syn case match

syn keyword kittenBool false true
syn keyword kittenKeyword case choice data def default else if infix infix_left infix_right import match option
syn keyword kittenTodo FIXME HACK NOTE TODO XXX

syn match kittenEscape /\\"/
syn match kittenEscape /\\\\/
syn match kittenEscape /\\[abfnrtv]/

syn match kittenFloat /[+\-]\d\+\.\d\+/
syn match kittenFloat /\d\+\.\d\+/

syn match kittenIdent /[a-z_][0-9A-Za-z_]*/

syn match kittenInt /[+\-]0b[01]\+/
syn match kittenInt /0b[01]\+/

syn match kittenInt /[+\-]0o[0-7]\+/
syn match kittenInt /0o[0-7]\+/

syn match kittenInt /[+\-]0x[0-9A-Fa-f]\+/
syn match kittenInt /0x[0-9A-Fa-f]\+/

syn match kittenInt /[+\-]\d\+/
syn match kittenInt /\d\+/

syn match kittenIntrinsic /__[0-9a-z_]\+/
syn match kittenSymbol /[!#$%&*+-./<=>?@^|~]\+/
syn match kittenType /[A-Z][0-9A-Za-z_]*/

syn region kittenComment start="//" end="$" contains=kittenTodo
syn region kittenMultiComment start="/\*" end="\*/" contains=kittenTodo,kittenMultiComment
syn region kittenString start='"' end='"' contains=kittenEscape

let b:current_syntax = "kitten"

hi def link kittenBool Boolean
hi def link kittenComment Comment
hi def link kittenEscape Character
hi def link kittenFloat Float
hi def link kittenIdent Identifier
hi def link kittenInt Number
hi def link kittenIntrinsic Keyword
hi def link kittenKeyword Keyword
hi def link kittenMultiComment Comment
hi def link kittenString String
hi def link kittenSymbol Operator
hi def link kittenTodo Todo
hi def link kittenType Type
