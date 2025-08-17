# META
~~~ini
description=Bug test: Simple lambda expression
type=expr
~~~

# SOURCE
~~~roc
|x| x
~~~

# TOKENS
~~~zig
OpBar(0-1),LowerIdent(1-2),OpBar(2-3),LowerIdent(4-5),EndOfFile(5-5)
~~~

# PARSE_AST2
~~~clojure
(lambda [(lc "x" @1)] (lc "x" @4) @0)

~~~
