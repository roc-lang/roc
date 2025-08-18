# META
~~~ini
description=Test big string literal
type=expr
~~~

# SOURCE
~~~roc
"This is a long string that should trigger the big string path"
~~~

# TOKENS
~~~zig
StringStart(0-1),StringPart(1-62),StringEnd(62-63),EndOfFile(63-63)
~~~

# PARSE_AST2
~~~clojure
(str "This is a long string that should trigger the big string path" @0)

~~~
