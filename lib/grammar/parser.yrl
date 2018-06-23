Nonterminals 
word 
list
list_elems 
elem 
range
set_elems
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
'?'
'.'
escape
atom.

Rootsymbol elem.

elem -> list                     : {list, '$1'}. 
elem -> set                      : {set, '$1'}.
elem -> word                     : {word, '$1'}.
elem -> repzero                  : {repzero, '$1'}.
elem -> reponce                  : {reponce, '$1'}.
elem -> optelem                  : {optelem, '$1'}.
elem -> word set                 : {setword, '$1', '$2'}.
elem -> '.'                      : {wildcard, '.'}.

list_elems -> elem               : ['$1']. 
list_elems -> elem '|' list_elems: ['$1' | '$3' ].

set_elems -> elem                : ['$1']. 
set_elems -> elem set_elems      : ['$1' | '$2' ].

list -> '(' ')'                  : [].
list -> '(' list_elems ')'       : '$2'.

set -> '[' ']'                   : [].
set -> '[' set_elems ']'         : '$2'.

word -> atom                     : {atom, extract_token('$1')}.
word -> escape                   : {escape, extract_token('$1')}.
word -> range                    : {range, '$1'}.
word -> word word                : ['$1' | '$2'].

range -> atom '-' atom           : {extract_token('$1'), extract_token('$3')}.

repzero -> elem '*'              : '$1'.
reponce -> elem '+'              : '$1'.
optelem -> elem '?'              : '$1'.

Erlang code.
extract_token({_Token, _Line, Value}) -> Value.
