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
(module-header
  (exposes
    (lc "person")

    (lc "final")
))
~~~
# FORMATTED
~~~roc
module [person, final]

person = { name : "Alice", age : 30, city : "Boston" }
updated_one = { ..person }
age : 31
}
updated2 = { ..updated_one }
city : "New York"
}
final = { ..updated2 }
name : ("Alice Smith", age) : 32
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 4:15 to 4:27

**Parse Error**
at 4:35 to 5:1

**Parse Error**
at 5:12 to 5:29

**Parse Error**
at 5:46 to 6:1

**Parse Error**
at 6:9 to 6:23

**Parse Error**
at 6:52 to 6:53

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
      (Expr.binop_colon
        (Expr.lookup "city")
        (Expr.str_literal_big)
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "updated_one")
    (Expr.record_literal
      (Expr.unary_double_dot)
    )
  )
  (Expr.binop_colon
    (Expr.lookup "age")
    (Expr.num_literal_i32 31)
  )
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.lookup "updated2")
    (Expr.record_literal
      (Expr.unary_double_dot)
    )
  )
  (Expr.binop_colon
    (Expr.lookup "city")
    (Expr.str_literal_big)
  )
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.lookup "final")
    (Expr.record_literal
      (Expr.unary_double_dot)
    )
  )
  (Expr.binop_colon
    (Expr.lookup "name")
    (Expr.binop_colon
      (Expr.tuple_literal
        (Expr.str_literal_big)
        (Expr.lookup "age")
      )
      (Expr.num_literal_i32 32)
    )
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
updated_one : {}
updated2 : {}
final : {}
~~~
