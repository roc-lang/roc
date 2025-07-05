# META
~~~ini
description=Invalid integer literal that exceeds i128 range
type=expr
~~~
# SOURCE
~~~roc
99999999999999999999999999999999999999999
~~~
# EXPECTED
invalid_num_literal - expr_int_invalid.md:1:1:1:42
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:42),EndOfFile(1:42-1:42),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.42 (raw "99999999999999999999999999999999999999999"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "invalid_num_literal"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.42 (type "Error"))
~~~
