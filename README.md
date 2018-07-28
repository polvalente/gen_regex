# GenRegex

This project attemps to generate valid strings for a given regular expression.

## Unsupported Regular Expression elements

Currently, there are some features of PCRE that aren't supported yet. They are:

- Lookahead and Lookbehind references (e.g. `~r/q(?!u)/` and `~r/q(?=u)/`)

- Match names (e.g. `~r/(?<foo>abc)/`)

- Backreferences

- Anchors

- Mode modifiers

Also, it is important to highlight the fact that any negated character classes like `~r/[^bar]/`
and any wildcards result in elements taken from the subset of printable ASCII characters.