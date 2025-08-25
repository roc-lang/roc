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
    (block
      (binop_colon
        (lc "age")
        (binop_colon
          (tuple_literal
            (num_literal_i32 30)
            (lc "city")
          )
          (str_literal_small "NYC")
        )
      )
    )
  )
  (binop_equals
    (lc "list")
    (list_literal
      (tuple_literal
        (num_literal_i32 1)
        (num_literal_i32 2)
        (num_literal_i32 3)
      )
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
isActive = Bool | .true
add = \(a, b) -> a + b
pair = (1, "hello")
record = {
	age: ((30, city): "NYC")
}
list = [(1, 2, 3)]
y = x + 10
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 5:16 to 5:20

**Unsupported Node**
at 6:11 to 6:18

**Unsupported Node**
at 7:23 to 7:24

**Unsupported Node**
at 8:29 to 8:30

**Unsupported Node**
at 9:12 to 10:1

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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
x : Num(_size)
name : Str
pi : F64
isActive : _arg -> _ret
add : Error
pair : Error
record : {}
list : Error
y : _c
~~~
