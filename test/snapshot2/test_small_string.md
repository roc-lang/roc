# META
~~~ini
description=Test small string literal
type=expr
~~~

# SOURCE
~~~roc
"Hi"
~~~

# TOKENS
~~~zig
StringStart(0-1),StringPart(1-3),StringEnd(3-4),EndOfFile(4-4)
~~~

# PARSE_AST2
~~~clojure
(str "Hi" @0)

~~~
