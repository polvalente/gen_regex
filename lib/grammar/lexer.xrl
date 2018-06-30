Definitions.

COMMA                = ,(\s)*
ATOM                 = .

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
\-                   : {token, {'-', TokenLine}}.
\.                   : {token, {'.', TokenLine}}.
\?                   : {token, {'?', TokenLine}}.
\\.                  : {token, {escape, TokenLine, TokenChars}}.
{COMMA}              : {token, {comma, TokenLine, TokenChars}}.
{ATOM}               : {token, {atom, TokenLine, TokenChars}}.

Erlang code.
