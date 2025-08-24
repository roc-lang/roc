# META
~~~ini
description=Test five character string (requires big)
type=expr
~~~

# SOURCE
~~~roc
"ABCDE"
~~~

# TOKENS
~~~zig
StringStart(0-1),StringPart(1-6),StringEnd(6-7),EndOfFile(7-7)
~~~

# PARSE_AST2
~~~clojure
(str "ABCDE" @0)

~~~
