# META
~~~ini
description=string_without_escape
type=expr
~~~
# SOURCE
~~~roc
"123 abc 456 def"
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:17),StringEnd(1:17-1:18),EndOfFile(1:18-1:18),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.18
	(e-string-part @1.2-1.17 (raw "123 abc 456 def")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-1.18 (id 74)
	(e-literal @1.2-1.17 (string "123 abc 456 def")))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Str"))
~~~
