Definitions.

ATOM                 = .

Rules.
\[                   : {token, {'[', TokenLine}}.
\]                   : {token, {']', TokenLine}}.
\(                   : {token, {'(', TokenLine}}.
\)                   : {token, {')', TokenLine}}.
\|                   : {token, {'|', TokenLine}}.
\*                   : {token, {'*', TokenLine}}.
\+                   : {token, {'+', TokenLine}}.
\-                   : {token, {'-', TokenLine}}.
\,                   : {token, {',', TokenLine}}.
\.                   : {token, {'.', TokenLine}}.
\?                   : {token, {'?', TokenLine}}.
\\.                  : {token, {escape, TokenLine, TokenChars}}.
{ATOM}               : {token, {atom, TokenLine, TokenChars}}.

Erlang code.
