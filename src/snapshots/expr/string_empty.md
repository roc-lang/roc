# META
~~~ini
description=Empty string literal
type=expr
~~~
# SOURCE
~~~roc
""
~~~
# PROBLEMS
~~~txt
NIL
~~~
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:2),StringEnd(1:2-1:3),EndOfFile(1:3-1:3),
~~~
# PARSE
~~~clojure
(string (1:1-1:3) (string_part (1:2-1:2) ""))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_string (1:1-1:3) (e_literal (1:2-1:2) ""))
~~~
# TYPES
~~~clojure
(expr 13 (type "Str"))
~~~