# META
~~~ini
description=Unary not operation expression
type=expr
~~~
# SOURCE
~~~roc
!isValid
~~~
# PROBLEMS
~~~txt
PARSER: expr_unexpected_token
~~~
# TOKENS
~~~zig
OpBang(1:1-1:2),LowerIdent(1:2-1:9),EndOfFile(1:9-1:9),
~~~
# PARSE
~~~clojure
(malformed_expr (1:1-1:2) "expr_unexpected_token")
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