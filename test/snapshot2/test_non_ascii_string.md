# META
~~~ini
description=Test non-ASCII string (requires big even if short)
type=expr
~~~

# SOURCE
~~~roc
"ñ"
~~~

# TOKENS
~~~zig
StringStart(0-1),StringPart(1-3),StringEnd(3-4),EndOfFile(4-4)
~~~

# PARSE_AST2
~~~clojure
(str "ñ" @0)

~~~
