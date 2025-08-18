# META
~~~ini
description=Test underscore pattern
type=expr
~~~

# SOURCE
~~~roc
|_| 42
~~~

# TOKENS
~~~zig
OpBar(0-1),Underscore(1-2),OpBar(2-3),Int(4-6),EndOfFile(6-6)
~~~

# PARSE_AST2
~~~clojure
(lambda [(_ @1)] (num 42 @4) @0)

~~~
