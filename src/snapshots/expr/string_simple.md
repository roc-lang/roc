# META
~~~ini
description=Simple string literal
type=expr
~~~
# SOURCE
~~~roc
"hello world"
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:13),StringEnd(1:13-1:14),EndOfFile(1:14-1:14),
~~~
# PARSE
~~~clojure
(string (1:1-1:14) (string_part (1:2-1:13) "hello world"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_string (1:1-1:14) (e_literal (1:2-1:13) "hello world"))
~~~
# TYPES
~~~clojure
(expr 13 (type "Str"))
~~~