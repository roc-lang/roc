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
OpenCurly LineComment LowerIdent OpAssign OpBar Underscore OpBar OpenCurly CloseCurly LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpAssign OpenRound CloseRound LowerIdent OpAssign OpenRound Int CloseRound LowerIdent OpAssign OpenRound Int Comma Int CloseRound LowerIdent OpAssign OpenRound Int Comma String Comma UpperIdent CloseRound LowerIdent OpAssign OpenRound OpenRound Int Comma Int CloseRound Comma OpenRound Int Comma Int CloseRound CloseRound LowerIdent OpAssign OpenRound LowerIdent OpenRound Int CloseRound Comma String Comma OpenSquare Int Comma Int Comma Int CloseSquare CloseRound LowerIdent OpAssign OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpenRound OpBar LowerIdent OpBar LowerIdent OpPlus Int Comma Int CloseRound BlankLine LowerIdent CloseCurly ~~~
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
add_one = |_| {}
x = 10
y = 20
z = 30
# example tuples
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
EMPTY TUPLE NOT ALLOWED - tuple_comprehensive.md:9:10:9:12
UNUSED VARIABLE - tuple_comprehensive.md:10:2:10:8
UNUSED VARIABLE - tuple_comprehensive.md:11:2:11:6
UNUSED VARIABLE - tuple_comprehensive.md:12:2:12:8
UNUSED VARIABLE - tuple_comprehensive.md:13:2:13:8
UNUSED VARIABLE - tuple_comprehensive.md:14:2:14:7
UNUSED VARIABLE - tuple_comprehensive.md:15:2:15:11
UNUSED VARIABLE - tuple_comprehensive.md:16:2:16:13
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "add_one"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.num_literal_i32 10)
  )
  (Stmt.assign
    (pattern (Patt.ident "y"))
    (Expr.num_literal_i32 20)
  )
  (Stmt.assign
    (pattern (Patt.ident "z"))
    (Expr.num_literal_i32 30)
  )
  (Stmt.assign
    (pattern (Patt.ident "empty"))
    (Expr.tuple_literal
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "single"))
    (Expr.num_literal_i32 42)
  )
  (Stmt.assign
    (pattern (Patt.ident "pair"))
    (Expr.tuple_literal
      (Expr.num_literal_i32 1)
      (Expr.num_literal_i32 2)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "triple"))
    (Expr.tuple_literal
      (Expr.num_literal_i32 1)
      (Expr.str_literal_big)
      (Expr.tag_no_args)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "nested"))
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
  (Stmt.assign
    (pattern (Patt.ident "mixed"))
    (Expr.tuple_literal
      (Expr.fn_call)
      (Expr.str_literal_big)
      (Expr.list_literal)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "with_vars"))
    (Expr.tuple_literal
      (Expr.lookup "x")
      (Expr.lookup "y")
      (Expr.lookup "z")
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "with_lambda"))
    (Expr.lambda (canonicalized))
  )
  (Expr.lookup "empty")
)
~~~
# SOLVED
~~~clojure
; Total type variables: 84
(var #0 _)
(var #1 -> #71)
(var #2 _)
(var #3 -> #70)
(var #4 -> #71)
(var #5 _)
(var #6 -> #7)
(var #7 Num *)
(var #8 _)
(var #9 -> #10)
(var #10 Num *)
(var #11 _)
(var #12 -> #13)
(var #13 Num *)
(var #14 _)
(var #15 -> #72)
(var #16 -> #72)
(var #17 _)
(var #18 -> #19)
(var #19 Num *)
(var #20 _)
(var #21 -> #73)
(var #22 Num *)
(var #23 Num *)
(var #24 -> #73)
(var #25 _)
(var #26 -> #74)
(var #27 Num *)
(var #28 Str)
(var #29 _)
(var #30 -> #74)
(var #31 _)
(var #32 -> #77)
(var #33 Num *)
(var #34 Num *)
(var #35 -> #75)
(var #36 Num *)
(var #37 Num *)
(var #38 -> #76)
(var #39 -> #77)
(var #40 _)
(var #41 -> #79)
(var #42 -> #78)
(var #43 Num *)
(var #44 _)
(var #45 Str)
(var #46 Num *)
(var #47 Num *)
(var #48 Num *)
(var #49 _)
(var #50 -> #79)
(var #51 _)
(var #52 -> #80)
(var #53 _)
(var #54 _)
(var #55 _)
(var #56 -> #80)
(var #57 _)
(var #58 -> #83)
(var #59 _)
(var #60 -> #61)
(var #61 -> #62)
(var #62 Num *)
(var #63 Num *)
(var #64 -> #82)
(var #65 -> #83)
(var #66 _)
(var #67 _)
(var #68 _)
(var #69 _)
(var #70 {})
(var #71 fn_pure)
(var #72 tuple)
(var #73 tuple)
(var #74 tuple)
(var #75 tuple)
(var #76 tuple)
(var #77 tuple)
(var #78 fn_pure)
(var #79 tuple)
(var #80 tuple)
(var #81 _)
(var #82 tuple)
(var #83 fn_pure)
~~~
# TYPES
~~~roc
pair : (Num(_size), Num(_size2))
with_vars : (_field, _field2, _field3)
n : _a
x : Num(_size)
y : Num(_size)
empty : ()
z : Num(_size)
triple : (Num(_size), Str, _field)
single : Num(_size)
nested : ((Num(_size), Num(_size2)), (Num(_size3), Num(_size4)))
add_one : _arg -> {}
with_lambda : _arg -> (Num(_size), Num(_size2))
mixed : (_field, Str, _field2)
~~~
