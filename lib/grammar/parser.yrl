Nonterminals 
word 
list
elems 
elem 
range
set.

Terminals 
'(' 
')' 
'['
']'
'|' 
'-' 
escape 
atom.

Rootsymbol list.

list -> '(' ')'             : [].
list -> '(' elems ')'       : '$2'.

elems -> elem               : ['$1']. 
elems -> elem '|' elems     : ['$1' | '$3' ].

elem -> word                : {word, '$1'}.
elem -> list                : '$1'. 
elem -> range               : {range, '$1'}.
elem -> set                 : {set, '$1'}.

word -> atom                : extract_token('$1').
word -> escape              : extract_token('$1').
word -> word word           : ['$1' | '$2'].

range -> atom '-' atom      : {extract_token('$1'), extract_token('$3')}.

set -> '[' ']'              : [].
set -> '['  elem ']'      : '$2'.

Erlang code.
extract_token({_Token, _Line, Value}) -> Value.
