# META
~~~ini
description=Comprehensive tuple expression tests
type=expr
~~~
# SOURCE
~~~roc
{
    # define these to avoid runtime errors
    add_one = |_| {}
    x = 10
    y = 20
    z = 30

    # example tuples
	empty = ()
	single = (42)
	pair = (1, 2)
	triple = (1, "hello", True)
	nested = ((1, 2), (3, 4))
	mixed = (add_one(5), "world", [1, 2, 3])
	with_vars = (x, y, z)
	with_lambda = (|n| n + 1, 42)

	empty
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign OpBar Underscore OpBar OpenCurly CloseCurly LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign OpenRound CloseRound LowerIdent OpAssign OpenRound Int CloseRound LowerIdent OpAssign OpenRound Int Comma Int CloseRound LowerIdent OpAssign OpenRound Int Comma String Comma UpperIdent CloseRound LowerIdent OpAssign OpenRound OpenRound Int Comma Int CloseRound Comma OpenRound Int Comma Int CloseRound CloseRound LowerIdent OpAssign OpenRound LowerIdent OpenRound Int CloseRound Comma String Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound LowerIdent OpAssign OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpenRound OpBar LowerIdent OpBar LowerIdent OpPlus Int Comma Int CloseRound LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "add_one")
    (lambda
      (body
        (record_literal)
      )
      (args
        (underscore)
      )
    )
  )
  (binop_equals
    (lc "x")
    (num_literal_i32 10)
  )
  (binop_equals
    (lc "y")
    (num_literal_i32 20)
  )
  (binop_equals
    (lc "z")
    (num_literal_i32 30)
  )
  (binop_equals
    (lc "empty")
    (tuple_literal)
  )
  (binop_equals
    (lc "single")
    (num_literal_i32 42)
  )
  (binop_equals
    (lc "pair")
    (tuple_literal
      (num_literal_i32 1)
      (num_literal_i32 2)
    )
  )
  (binop_equals
    (lc "triple")
    (tuple_literal
      (num_literal_i32 1)
      (str_literal_big "hello")
      (uc "True")
    )
  )
  (binop_equals
    (lc "nested")
    (tuple_literal
      (tuple_literal
        (num_literal_i32 1)
        (num_literal_i32 2)
      )
      (tuple_literal
        (num_literal_i32 3)
        (num_literal_i32 4)
      )
    )
  )
  (binop_equals
    (lc "mixed")
    (tuple_literal
      (apply_lc
        (lc "add_one")
        (num_literal_i32 5)
      )
      (str_literal_big "world")
      (list_literal
        (tuple_literal
          (num_literal_i32 1)
          (num_literal_i32 2)
          (num_literal_i32 3)
        )
      )
    )
  )
  (binop_equals
    (lc "with_vars")
    (tuple_literal
      (lc "x")
      (lc "y")
      (lc "z")
    )
  )
  (binop_equals
    (lc "with_lambda")
    (lambda
      (body
        (tuple_literal
          (binop_plus
            (lc "n")
            (num_literal_i32 1)
          )
          (num_literal_i32 42)
        )
      )
      (args
        (lc "n")
      )
    )
  )
  (lc "empty")
)
~~~
# FORMATTED
~~~roc
add_one = \_ -> {  }
x = 10
y = 20
z = 30

# example tuples
empty = ()
single = 42
pair = (1, 2)
triple = (1, "hello", True)
nested = ((1, 2), (3, 4))
mixed = (add_one(5), "world", [(1, 2, 3)])
with_vars = (x, y, z)
with_lambda = \n -> (n + 1, 42)

empty
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:15 to 3:19

**Unsupported Node**
at 9:10 to 9:11

**Unsupported Node**
at 11:14 to 11:15

**Unsupported Node**
at 12:28 to 12:29

**Unsupported Node**
at 13:26 to 13:27

**Unsupported Node**
at 14:41 to 15:1

**Unsupported Node**
at 15:22 to 15:23

**Unsupported Node**
at 16:17 to 16:21

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
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "empty")
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
add_one : Error
x : Num(_size)
y : Num(_size)
z : Num(_size)
empty : Error
single : Num(_size)
pair : Error
triple : Error
nested : Error
mixed : Error
with_vars : Error
with_lambda : Error
~~~
