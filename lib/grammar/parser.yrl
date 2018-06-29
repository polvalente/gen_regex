Nonterminals 
range
word 
option
opt_exprs 
elem 
elems
set_elem
set_elems
set
chr
repexpr
wrd_elem
expr
exprs.

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

Rootsymbol exprs.

expr -> elem                                : '$1'.
expr -> word                                : [{word, '$1'}].
exprs -> expr                               : '$1'.
exprs -> expr exprs                         : '$1' ++ '$2'.

elem -> set                                 : [{set, '$1'}].
elem -> option                              : [{option, '$1'}].
elem -> repexpr                             : ['$1'].
elem -> '.'                                 : [{wildcard, '.'}].

set -> '[' ']'                              : [].
set -> '[' set_elems ']'                    : '$2'.

set_elem  -> elem                           : '$1'.
set_elem  -> range                          : [{range, '$1'}].
set_elem  -> word                           : '$1'.
set_elems -> set_elem                       : '$1'.
set_elems -> set_elem set_elems             : '$1' ++ '$2'.

option -> '(' opt_exprs ')'                 : '$2'.
opt_exprs -> exprs                          : [{choice, '$1'}].
opt_exprs -> exprs '|' opt_exprs            : [{choice, '$1'}] ++ '$3'.

chr -> ''                                   : [{atom, ''}].
chr -> '-'                                  : [{atom, '-'}].
chr -> atom                                 : [{atom, extract_token('$1')}].
chr -> escape                               : [{escape, extract_token('$1')}].

word -> chr                                 : '$1'.
word -> word word                           : '$1' ++ '$2'.

range -> atom '-' atom                      : {extract_token('$1'), extract_token('$3')}.

wrd_elem -> word                            : [{word, '$1'}].
wrd_elem -> elem                            : '$1'.
repexpr -> wrd_elem '*'                     : {repexpr, ['$1', 0, nil]}.
repexpr -> wrd_elem '+'                     : {repexpr, ['$1', 1, nil]}.
repexpr -> wrd_elem '?'                     : {repexpr, ['$1', 0, 1]}.

Erlang code.
extract_token({_Token, _Line, Value}) -> Value.
