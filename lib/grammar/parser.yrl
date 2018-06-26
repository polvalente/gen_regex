Nonterminals 
word 
list
list_elems 
elem 
elems
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
''
escape
atom.

Rootsymbol elems.

elems -> elem                       : '$1'.
elems -> elem elems                 : '$1' ++ '$2'.

elem -> word                        : [{word, '$1'}].
elem -> list                        : [{list, '$1'}]. 
elem -> set                         : [{set, '$1'}].
elem -> repzero                     : ['$1'].
elem -> reponce                     : ['$1'].
elem -> optelem                     : ['$1'].
elem -> '.'                         : [{wildcard, '.'}].

list -> '(' ')'                     : [].
list -> '(' list_elems ')'          : '$2'.
list_elems -> elem                  : '$1'.
list_elems -> elem '|' list_elems   : '$1' ++ '$3'.

set -> '[' ']'                      : [].
set -> '[' elems ']'                : '$2'.

word -> ''                          : [{atom, ''}].
word -> atom                        : [{atom, extract_token('$1')}].
word -> escape                      : [{escape, extract_token('$1')}].
word -> range                       : ['$1'].
word -> word word                   : '$1' ++ '$2'.

range -> atom '-' atom              : {range, {extract_token('$1'), extract_token('$3')}}.

repzero -> elem '*'                 : {repzero, '$1'}.
reponce -> elem '+'                 : {reponce, '$1'}.
optelem -> elem '?'                 : {optelem, '$1'}.

Erlang code.
extract_token({_Token, _Line, Value}) -> Value.
