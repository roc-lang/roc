# META
~~~ini
description=Block with multiple assignments of different types
type=expr
~~~
# SOURCE
~~~roc
{
    x = 42
    name = "Alice"
    pi = 3.14159
    isActive = Bool.true
    add = |a, b| a + b
    pair = (1, "hello")
    record = { age: 30, city: "NYC" }
    list = [1, 2, 3]
    y = x + 10
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign String LowerIdent OpAssign Float LowerIdent OpAssign UpperIdent Dot LowerIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent OpPlus LowerIdent LowerIdent OpAssign OpenRound Int Comma String CloseRound LowerIdent OpAssign OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon String CloseCurly LowerIdent OpAssign OpenSquare Int Comma Int Comma Int CloseSquare LowerIdent OpAssign LowerIdent OpPlus Int CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "x")
    (num_literal_i32 42)
  )
  (binop_equals
    (lc "name")
    (str_literal_big "Alice")
  )
  (binop_equals
    (lc "pi")
    (frac_literal_big big:hello)
  )
  (binop_equals
    (lc "isActive")
    (binop_pipe
      (uc "Bool")
      (dot_lc "true")
    )
  )
  (binop_equals
    (lc "add")
    (lambda
      (body
        (binop_plus
          (lc "a")
          (lc "b")
        )
      )
      (args
        (tuple_literal
          (lc "a")
          (lc "b")
        )
      )
    )
  )
  (binop_equals
    (lc "pair")
    (tuple_literal
      (num_literal_i32 1)
      (str_literal_big "hello")
    )
  )
  (binop_equals
    (lc "record")
    (record_literal
      (binop_colon
        (lc "age")
        (num_literal_i32 30)
      )
      (binop_colon
        (lc "city")
        (str_literal_small "NYC")
      )
    )
  )
  (binop_equals
    (lc "list")
    (list_literal
      (num_literal_i32 1)
      (num_literal_i32 2)
      (num_literal_i32 3)
    )
  )
  (binop_equals
    (lc "y")
    (binop_plus
      (lc "x")
      (num_literal_i32 10)
    )
  )
)
~~~
# FORMATTED
~~~roc
x = 42
name = "Alice"
pi = 3.14159
isActive = Bool.true
add = |a, b| a + b
pair = (1, "hello")
record = { age : 30, city : "NYC" }
list = [1, 2, 3]
y = x + 10
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_c")
~~~
# TYPES
~~~roc
_c
~~~
