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
chr
repzero
reponce
optelem
wrd_elem.

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
elems -> word                                : [{word, '$1'}].
elems -> elems elems                         : '$1' ++ '$2'.

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
set_elem  -> word                           : '$1'.
set_elems -> set_elem                       : '$1'.
set_elems -> set_elems set_elems            : '$1' ++ '$2'.

option -> '(' opt_elems ')'                 : '$2'.
opt_elems -> elems                          : [{choice, '$1'}].
opt_elems -> opt_elems '|' opt_elems        : '$1' ++ '$3'.

chr -> ''                                   : [{atom, ''}].
chr -> '-'                                  : [{atom, '-'}].
chr -> atom                                 : [{atom, extract_token('$1')}].
chr -> escape                               : [{escape, extract_token('$1')}].

word -> chr                                 : '$1'.
word -> word word                           : '$1' ++ '$2'.

range -> atom '-' atom                      : {extract_token('$1'), extract_token('$3')}.

wrd_elem -> word                            : [{word, '$1'}].
wrd_elem -> elem                            : '$1'.
repzero -> wrd_elem '*'                     : {repzero, '$1'}.
reponce -> wrd_elem '+'                     : {reponce, '$1'}.
optelem -> wrd_elem '?'                     : {optelem, '$1'}.

Erlang code.
extract_token({_Token, _Line, Value}) -> Value.
