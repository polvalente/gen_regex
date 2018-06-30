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
\\.                  : {token, {escape, TokenLine, TokenChars}}.
{RANGE}              : {token, {range, TokenLine, TokenChars}}.
{COMMA}              : {token, {comma, TokenLine, TokenChars}}.
{ATOM}               : {token, {atom, TokenLine, TokenChars}}.

Erlang code.
