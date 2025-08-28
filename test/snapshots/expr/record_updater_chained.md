# META
~~~ini
description=Chained record updater expressions
type=file
~~~
# SOURCE
~~~roc
module [person, final]

person = { name: "Alice", age: 30, city: "Boston" }
updated_one = { ..person, age: 31 }
updated2 = { ..updated_one, city: "New York" }
final = { ..updated2, name: "Alice Smith", age: 32 }
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int Comma LowerIdent OpColon String CloseCurly LowerIdent OpAssign OpenCurly DoubleDot LowerIdent Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign OpenCurly DoubleDot LowerIdent Comma LowerIdent OpColon String CloseCurly LowerIdent OpAssign OpenCurly DoubleDot LowerIdent Comma LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "person")
    (record_literal
      (binop_colon
        (lc "name")
        (str_literal_big "Alice")
      )
      (binop_colon
        (lc "age")
        (num_literal_i32 30)
      )
      (binop_colon
        (lc "city")
        (str_literal_big "Boston")
      )
    )
  )
  (binop_equals
    (lc "updated_one")
    (record_literal
      (double_dot_lc "person")
    )
  )
  (binop_colon
    (lc "age")
    (num_literal_i32 31)
  )
  (malformed malformed:expr_unexpected_token)
  (binop_equals
    (lc "updated2")
    (record_literal
      (double_dot_lc "updated_one")
    )
  )
  (binop_colon
    (lc "city")
    (str_literal_big "New York")
  )
  (malformed malformed:expr_unexpected_token)
  (binop_equals
    (lc "final")
    (record_literal
      (double_dot_lc "updated2")
    )
  )
  (binop_colon
    (lc "name")
    (binop_colon
      (tuple_literal
        (str_literal_big "Alice Smith")
        (lc "age")
      )
      (num_literal_i32 32)
    )
  )
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
module [person, final]

person = { name : "Alice", age : 30, city : "Boston" }
updated_one = { ..person }
age : 31
updated2 = { ..updated_one }
city : "New York"
final = { ..updated2 }
name : ("Alice Smith", age) : 32
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 4:15 to 4:27

**Parse Error**
at 4:35 to 4:35

**Parse Error**
at 5:12 to 5:29

**Parse Error**
at 5:46 to 5:46

**Parse Error**
at 6:9 to 6:23

**Parse Error**
at 6:52 to 6:52

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
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
