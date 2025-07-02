# META
~~~ini
description=control_characters_in_scalar
type=expr
~~~
# SOURCE
~~~roc
''
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
SingleQuote(1:1-1:4),EndOfFile(1:4-1:4),
~~~
# PARSE
~~~clojure
(e-single-quote @1.1-1.4 (raw "''"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.4 (value "7"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "Num(*)"))
~~~
