# META
~~~ini
description=Basic record updater with field override
type=file
~~~
# SOURCE
~~~roc
module [person, updated]

person = { name: "Alice", age: 30 }
updated = { ..person, age: 31 }
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign OpenCurly DoubleDot LowerIdent Comma LowerIdent OpColon Int CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "person")
    (block
      (binop_colon
        (lc "name")
        (binop_colon
          (tuple_literal
            (str_literal_big "Alice")
            (lc "age")
          )
          (num_literal_i32 30)
        )
      )
    )
  )
  (binop_equals
    (lc "updated")
    (record_literal
      (double_dot_lc "person")
    )
  )
  (binop_colon
    (lc "age")
    (num_literal_i32 31)
  )
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
module [
	person, updated
]


person = {
	name: (("Alice", age): 30)
}
updated = { ..person }
age: 31<malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 4:11 to 4:23

**Parse Error**
at 4:31 to 4:31

**Unsupported Node**
at 3:30 to 3:31

**Unsupported Node**
at 4:11 to 4:19

**Unsupported Node**
at 4:31 to 4:31

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "age")
    (Expr.num_literal_i32 31)
  )
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
