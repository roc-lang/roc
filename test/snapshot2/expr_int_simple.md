# META
~~~ini
description=Simple integer literal
type=expr
~~~

# SOURCE
~~~roc
42
~~~

# TOKENS
~~~zig
Int(0-2),EndOfFile(2-2)
~~~

# PARSE_AST2
~~~clojure
(num 42 @0)

~~~
