Nonterminals 
word 
list
elems 
elem 
range
set
repzero
reponce
optelem.

Terminals 
'(' 
')' 
'['
']'
'|' 
'-' 
'*'
'+'
'.'
'?'
escape
atom.

Rootsymbol elem.

elem -> list                : '$1'. 
elem -> set                 : {set, '$1'}.
elem -> word                : {word, '$1'}.
elem -> range               : {range, '$1'}.
elem -> repzero             : {repzero, '$1'}.
elem -> reponce             : {reponce, '$1'}.
elem -> optelem             : {optelem, '$1'}.

elems -> elem               : ['$1']. 
elems -> elem '|' elems     : ['$1' | '$3' ].

list -> '(' ')'             : [].
list -> '(' elems ')'       : '$2'.

set -> '[' ']'              : [].
set -> '[' elems ']'        : '$2'.

word -> atom                : {atom, extract_token('$1')}.
word -> escape              : {escape, extract_token('$1')}.
word -> '.'                 : {wildcard, '.'}.
word -> word word           : ['$1' | '$2'].

range -> atom '-' atom      : {extract_token('$1'), extract_token('$3')}.

repzero -> elem '*'         : '$1'.
reponce -> elem '+'         : '$1'.
optelem -> elem '?'         : '$1'.

Erlang code.
extract_token({_Token, _Line, Value}) -> Value.
