Definitions.

COMMA                = ,(\s)*
ATOM                 = .
RANGE                = .-.

Rules.
\[                   : {token, {'[', TokenLine}}.
\]                   : {token, {']', TokenLine}}.
\{                   : {token, {'{', TokenLine}}.
\}                   : {token, {'}', TokenLine}}.
\(                   : {token, {'(', TokenLine}}.
\)                   : {token, {')', TokenLine}}.
\|                   : {token, {'|', TokenLine}}.
\*                   : {token, {'*', TokenLine}}.
\+                   : {token, {'+', TokenLine}}.
\.                   : {token, {'.', TokenLine}}.
\?                   : {token, {'?', TokenLine}}.
\^                   : {token, {'^', TokenLine}}.
(\n|\r|\t|\v|\f)     : {token, {atom, TokenLine, TokenChars}}.
\\.                  : {token, {escape, TokenLine, TokenChars}}.
{RANGE}              : {token, {range, TokenLine, TokenChars}}.
{COMMA}              : {token, {comma, TokenLine, TokenChars}}.
{ATOM}               : {token, {atom, TokenLine, TokenChars}}.

Erlang code.
