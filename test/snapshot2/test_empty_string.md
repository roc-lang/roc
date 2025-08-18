# META
~~~ini
description=Test empty string literal
type=expr
~~~

# SOURCE
~~~roc
""
~~~

# TOKENS
~~~zig
StringStart(0-1),StringPart(1-1),StringEnd(1-2),EndOfFile(2-2)
~~~

# PARSE_AST2
~~~clojure
(str "" @0)

~~~
