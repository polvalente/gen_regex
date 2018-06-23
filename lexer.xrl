Definitions.

ATOM                 = .

Rules.
\[                   : {token, {'['}}.
\]                   : {token, {']'}}.
\(                   : {token, {'('}}.
\)                   : {token, {')'}}.
\|                   : {token, {'|'}}.
\*                   : {token, {'*'}}.
\+                   : {token, {'+'}}.
\-                   : {token, {'-'}}.
\.                   : {token, {'.'}}.
:\.                   : {token, {escape, TokenChars}}.
{ATOM}               : {token, {atom, TokenChars}}.

Erlang code.
