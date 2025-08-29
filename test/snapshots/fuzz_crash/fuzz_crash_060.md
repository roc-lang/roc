# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]C:k||match 0{0|#
0"
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare UpperIdent OpColon LowerIdent OpOr KwMatch Int OpenCurly Int OpBar Int MalformedString CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (uc "C")
    (binop_or
      (lc "k")
      (match
        (scrutinee           (num_literal_i32 0)
))
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

C : k || match 0
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:2 to 2:2

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.binop_or
      (Expr.lookup "k")
      (Expr.match)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag record_literal :type "{}")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
