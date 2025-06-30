# META
~~~ini
description=str_over_large_unicode_escape fail
type=expr
~~~
# SOURCE
~~~roc
'\u(FFFFFFFFF)'
~~~
# PROBLEMS
**INVALID SCALAR**
I am part way through parsing this scalar literal (character literal), but it appears to be invalid.

# TOKENS
~~~zig
SingleQuote(1:1-1:16),EndOfFile(1:16-1:16),
~~~
# PARSE
~~~clojure
(e-single-quote @1.1-1.16 (raw "'\u(FFFFFFFFF)'"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "invalid_single_quote"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.16 (type "Error"))
~~~
