# META
~~~ini
description=Invalid float literal too many decimal points
type=expr
~~~
# SOURCE
~~~roc
3.14.15
~~~
# PROBLEMS
PARSER: expr_no_space_dot_int
# TOKENS
~~~zig
Float(1:1-1:5),NoSpaceDotInt(1:5-1:8),EndOfFile(1:8-1:8),
~~~
# PARSE
~~~clojure
(malformed_expr (1:5-1:8) "expr_no_space_dot_int")
~~~
# FORMATTED
~~~roc

~~~
# CANONICALIZE
~~~clojure
(can_ir "empty")
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~