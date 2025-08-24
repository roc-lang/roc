# META
~~~ini
description=Minimal test - underscore type should become error type
type=file
~~~
# SOURCE
~~~roc
module []

BadType := _
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare UpperIdent OpColonEqual Underscore ~~~
# PARSE
~~~clojure
(block
  (binop_colon_equals
    (uc "BadType")
    (underscore)
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - minimal_underscore.md:1:1:1:1
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:13

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
