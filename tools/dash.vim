" Vim syntax file
" Language: dash
" Maintainer: Arne Schroppe
" Latest Revision: 2015

if exists("b:current_syntax")
  finish
endif



syn match dashSymbol ':[-a-zA-Z_][-a-zA-Z0-9_]\*'
syn match dashNumber '\<\d\+'
syn match dashString '"[^"]*"'
syn match dashBoolean '\(:true\)\|\(:false\)'
syn keyword dashKeyword module begin end match with do open if then else
syn match dashArrow '<-\|->'
syn region dashComment start='/--' end='--/'
syn match dashComment '--.*$'


hi def link dashKeyword Keyword
hi def link dashArrow Operator
hi def link dashBoolean Boolean
hi def link dashNumber Number
hi def link dashString String
hi def link dashSymbol Constant
hi def link dashComment Comment


