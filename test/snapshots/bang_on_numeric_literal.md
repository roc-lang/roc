# META
~~~ini
description=Bang operator on numeric literal should produce type error
type=expr
~~~
# SOURCE
~~~roc
!3
~~~
# EXPECTED
TYPE MISMATCH - bang_on_numeric_literal.md:1:2:1:3
# PROBLEMS
── ✗ type mismatch ────────────────────────────── bang_on_numeric_literal.md:1:2

This number is being used where a non-number type is needed.

!3
 ^

Other code expects this to have the type:

    Bool

# TOKENS
~~~zig
OpBang,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(unary "!"
	(e-int (raw "3")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call (constraint-fn-var 215)
	(e-lookup-associated-resolved (source "Bool.not") (builtin) (target-node "17379") (target-def "17379"))
	(e-runtime-error (tag "erroneous_value_expr")))
~~~
# TYPES
~~~clojure
(expr (type "Bool"))
~~~
