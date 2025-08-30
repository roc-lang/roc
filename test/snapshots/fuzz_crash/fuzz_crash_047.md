# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module [person, updated]

person = { name: "Alice", age: 30 }
updated = { ..person,
 age: 31 }
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign OpenCurly DoubleDot LowerIdent Comma LowerIdent OpColon Int CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "person")

    (lc "updated")
))
~~~
# FORMATTED
~~~roc
module [person, updated]

person = { name : "Alice", age : 30 }
updated = { ..person }
age : 31
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 4:11 to 5:2

**Parse Error**
at 5:10 to 5:11

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "person")
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "name")
        (Expr.str_literal_big)
      )
      (Expr.binop_colon
        (Expr.lookup "age")
        (Expr.num_literal_i32 30)
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "updated")
    (Expr.record_literal
      (Expr.unary_double_dot)
    )
  )
  (Expr.binop_colon
    (Expr.lookup "age")
    (Expr.num_literal_i32 31)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
person : {}
updated : {}
~~~
