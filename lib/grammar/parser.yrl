Nonterminals 
range
word 
option
opt_elems 
elem 
elems
set_elem
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

elems -> elem                               : '$1'.
elems -> elem elems                         : '$1' ++ '$2'.

elem -> word                                : [{word, '$1'}].
elem -> set                                 : [{set, '$1'}].
elem -> option                              : [{option, '$1'}].
elem -> repzero                             : ['$1'].
elem -> reponce                             : ['$1'].
elem -> optelem                             : ['$1'].
elem -> '.'                                 : [{wildcard, '.'}].

set -> '[' ']'                              : [].
set -> '[' set_elems ']'                    : '$2'.

set_elem  -> elem                           : '$1'.
set_elem  -> range                          : [{range, '$1'}].
set_elems -> set_elem                       : '$1'.
set_elems -> set_elem set_elems             : '$1' ++ '$2'.

option -> '(' opt_elems ')'                 : '$2'.
opt_elems -> elems                          : [{choice, '$1'}].
opt_elems -> elems '|' opt_elems            : [{choice, '$1'}] ++ '$3'.

word -> ''                                  : [{atom, ''}].
word -> '-'                                 : [{atom, '-'}].
word -> atom                                : [{atom, extract_token('$1')}].
word -> escape                              : [{escape, extract_token('$1')}].
word -> word word                           : '$1' ++ '$2'.

range -> atom '-' atom                      : {extract_token('$1'), extract_token('$3')}.

repzero -> elem '*'                         : {repzero, '$1'}.
reponce -> elem '+'                         : {reponce, '$1'}.
optelem -> elem '?'                         : {optelem, '$1'}.

Erlang code.
extract_token({_Token, _Line, Value}) -> Value.
