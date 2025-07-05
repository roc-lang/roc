# META
~~~ini
description=str_over_large_unicode_escape fail
type=expr
~~~
# SOURCE
~~~roc
'\u(FFFFFFFFF)'
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**INVALID SCALAR**
I am part way through parsing this scalar literal (character literal), but it contains more than one character.
A single-quoted literal must contain exactly one character, e.g. 'a'.

# TOKENS
~~~zig
SingleQuote(1:1-1:16),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-single-quote @1.1-1.16 (raw "'\u(FFFFFFFFF)'"))
~~~
# FORMATTED
~~~roc
'\u(FFFFFFFFF)'
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "too_long_single_quote"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.16 (type "Error"))
~~~
