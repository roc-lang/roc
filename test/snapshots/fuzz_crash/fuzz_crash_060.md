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
KwModule OpenSquare CloseSquare UpperIdent OpColon LowerIdent OpOr KwMatch Int OpenCurly Int OpBar LineComment Int MalformedString CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

C : k || match 0
#
0"
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:22 to 1:24

**Parse Error**
at 1:24 to 2:1

**Parse Error**
at 2:2 to 3:1

**Parse Error**
at 3:1 to 3:2

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.binop_or
      (Expr.lookup "k")
      (Expr.match)
    )
  )
  (Expr.malformed)
  (Expr.num_literal_i32 0)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
