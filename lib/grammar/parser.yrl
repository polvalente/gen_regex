Nonterminals
word
option
opt_exprs
elem
set_elem
set_elems
set
neg_set
chr
repexpr
wrd_elem
nemptywrd
expr
exprs
line.

Terminals
'('
')'
'{'
'}'
'['
']'
'|'
'-'
'*'
'+'
'?'
'.'
'^'
'$'
''
range
comma
escape
atom.

Rootsymbol line.

line -> '^' exprs '$'                                                   : '$2'.
line -> '^' exprs                                                       : '$2'.
line -> exprs '$'                                                       : '$1'.
line -> exprs                                                           : '$1'.

expr -> elem                                                            : '$1'.
expr -> word                                                            : [{word, '$1'}].
exprs -> expr                                                           : '$1'.
exprs -> expr exprs                                                     : '$1' ++ '$2'.

elem -> set                                                             : [{set, '$1'}].
elem -> neg_set                                                         : [{negset, '$1'}].
elem -> option                                                          : [{option, '$1'}].
elem -> repexpr                                                         : ['$1'].
elem -> '.'                                                             : [{wildcard, '.'}].

set -> '[' ']'                                                          : [].
set -> '[' set_elems ']'                                                : '$2'.
neg_set -> '[' '^' set_elems ']'                                        : '$3'.

set_elem  -> elem                                                       : '$1'.
set_elem  -> word                                                       : '$1'.
set_elems -> set_elem                                                   : '$1'.
set_elems -> set_elem set_elems                                         : '$1' ++ '$2'.

option -> '(' opt_exprs ')'                                             : '$2'.
opt_exprs -> exprs                                                      : [{choice, '$1'}].
opt_exprs -> exprs '|' opt_exprs                                        : [{choice, '$1'}] ++ '$3'.

chr -> '-'                                                              : [{atom, '-'}].
chr -> '^'                                                              : [{atom, '^'}].
chr -> atom                                                             : [{atom, extract_token('$1')}].
chr -> escape                                                           : [{escape, extract_token('$1')}].

word -> ''                                                              : [].
word -> comma                                                           : [{atom, extract_token('$1')}].
word -> range                                                           : [{range, extract_token('$1')}].
word -> chr                                                             : '$1'.
word -> word word                                                       : '$1' ++ '$2'.

nemptywrd -> chr                                                        : '$1'.
nemptywrd -> nemptywrd nemptywrd                                        : '$1' ++ '$2'.

wrd_elem -> word                                                        : [{word, '$1'}].
wrd_elem -> elem                                                        : '$1'.
repexpr -> wrd_elem '*'                                                 : {repexpr, ['$1', 0, nil]}.
repexpr -> wrd_elem '+'                                                 : {repexpr, ['$1', 1, nil]}.
repexpr -> wrd_elem '?'                                                 : {repexpr, ['$1', 0, 1]}.
repexpr -> wrd_elem '{' nemptywrd '}'                                   : {repexpr, ['$1', [{word, '$3'}], [{word, '$3'}]]}.
repexpr -> wrd_elem '{' nemptywrd comma'}'                              : {repexpr, ['$1', [{word, '$3'}], nil]}.
repexpr -> wrd_elem '{' nemptywrd comma nemptywrd '}'                   : {repexpr, ['$1', [{word, '$3'}], [{word, '$5'}]]}.

Erlang code.
extract_token({_Token, _Line, Value}) -> Value.
