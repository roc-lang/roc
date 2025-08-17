# META
~~~ini
description=Just a string literal expression
type=expr
~~~

# SOURCE
~~~roc
"Hello, World!"
~~~

# TOKENS
~~~zig
StringStart(0-1),StringPart(1-14),StringEnd(14-15),EndOfFile(15-15)
~~~

# PARSE_AST2
~~~clojure
(str_literal_big "<big>" @0)

~~~
