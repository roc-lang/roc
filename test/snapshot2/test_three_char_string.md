# META
~~~ini
description=Test three character string (max small)
type=expr
~~~

# SOURCE
~~~roc
"ABC"
~~~

# TOKENS
~~~zig
StringStart(0-1),StringPart(1-4),StringEnd(4-5),EndOfFile(5-5)
~~~

# PARSE_AST2
~~~clojure
(str "ABC" @0)

~~~
