# META
~~~ini
description=control_characters_in_scalar
type=expr
~~~
# SOURCE
~~~roc
''
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
SingleQuote(1:1-1:4),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-single-quote @1.1-1.4 (raw "''"))
~~~
# FORMATTED
~~~roc
''
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.4 (value "7"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "Num(*)"))
~~~
