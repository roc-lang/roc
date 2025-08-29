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
        (num_literal_i32 1)
        (num_literal_i32 2)
        (num_literal_i32 3)
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
# define these to avoid runtime errors
add_one = |_| {  }
x = 10
y = 20
z = 30
empty = ()
single = 42
pair = (1, 2)
triple = (1, "hello", True)
nested = ((1, 2), (3, 4))
mixed = (add_one(5), "world", [1, 2, 3])
with_vars = (x, y, z)
with_lambda = |n| (n + 1, 42)
empty
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "add_one")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "x")
    (Expr.num_literal_i32 10)
  )
  (Expr.binop_equals
    (Expr.lookup "y")
    (Expr.num_literal_i32 20)
  )
  (Expr.binop_equals
    (Expr.lookup "z")
    (Expr.num_literal_i32 30)
  )
  (Expr.binop_equals
    (Expr.lookup "empty")
    (Expr.tuple_literal
    )
  )
  (Expr.binop_equals
    (Expr.lookup "single")
    (Expr.num_literal_i32 42)
  )
  (Expr.binop_equals
    (Expr.lookup "pair")
    (Expr.tuple_literal
      (Expr.num_literal_i32 1)
      (Expr.num_literal_i32 2)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "triple")
    (Expr.tuple_literal
      (Expr.num_literal_i32 1)
      (Expr.str_literal_big)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "nested")
    (Expr.tuple_literal
      (Expr.tuple_literal
        (Expr.num_literal_i32 1)
        (Expr.num_literal_i32 2)
      )
      (Expr.tuple_literal
        (Expr.num_literal_i32 3)
        (Expr.num_literal_i32 4)
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "mixed")
    (Expr.tuple_literal
      (Expr.apply_ident)
      (Expr.str_literal_big)
      (Expr.list_literal)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "with_vars")
    (Expr.tuple_literal
      (Expr.lookup "x")
      (Expr.lookup "y")
      (Expr.lookup "z")
    )
  )
  (Expr.binop_equals
    (Expr.lookup "with_lambda")
    (Expr.lambda)
  )
  (Expr.lookup "empty")
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
