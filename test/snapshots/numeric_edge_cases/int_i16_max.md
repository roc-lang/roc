# META
~~~ini
description=Maximum value for i16 (32767)
type=expr
~~~
# SOURCE
~~~roc
32767
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-int (raw "32767"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-num (value "32767"))
~~~
# TYPES
~~~clojure
(expr (type "_a where [_b.from_num_literal : _arg -> _ret]"))
~~~
