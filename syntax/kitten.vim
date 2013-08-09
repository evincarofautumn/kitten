" Vim syntax file
" Language: Kitten
" Maintainer: Jon Purdy <evincarofautumn@gmail.com>

syntax clear

syn case match

syn keyword kittenBool false true
syn keyword kittenKeyword choice def else from if import option to
syn keyword kittenTodo /\(FIXME\|HACK\|NOTE\|TODO\|XXX\)/ contained
syn match kittenBuiltin /__[0-9a-z_]+/
syn match kittenComment /\/\/.*$/ contains=kittenTodo
syn match kittenFloat /\<[+-][0-9]+\.[0-9]+\>/
syn match kittenIdent /[a-z][0-9A-Za-z_]*/
syn match kittenInt /\<[+-][0-9]+\>/
syn match kittenSymbol /[!#$%&*+-./;<=>?@^\|~]+/
syn match kittenType /[A-Z][0-9A-Za-z_]*/
syn region kittenMultiComment start="/\*" end="\*/" contains=kittenTodo

let b:current_syntax = "kitten"

hi def link kittenBool Boolean
hi def link kittenComment Comment
hi def link kittenFloat Float
hi def link kittenIdent Identifier
hi def link kittenInt Number
hi def link kittenKeyword Keyword
hi def link kittenMultiComment Comment
hi def link kittenSymbol Define
hi def link kittenTodo Todo
hi def link kittenType Typedef
hi def link kittenBuiltin Keyword

