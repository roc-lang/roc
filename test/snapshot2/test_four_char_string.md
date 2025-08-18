# META
~~~ini
description=Test four character string (requires big)
type=expr
~~~

# SOURCE
~~~roc
"ABCD"
~~~

# TOKENS
~~~zig
StringStart(0-1),StringPart(1-5),StringEnd(5-6),EndOfFile(6-6)
~~~

# PARSE_AST2
~~~clojure
(str "ABCD" @0)

~~~
