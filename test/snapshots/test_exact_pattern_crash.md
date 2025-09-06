# META
~~~ini
description=Exact pattern from type_alias_parameterized with variations
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Type alias with parameters, just like the original
Pair(a, b) : (a, b)

# Function that uses the alias and will need instantiation
swap_pair : Pair(a, b) -> Pair(b, a)
swap_pair = |(x, y)| (y, x)

# Another polymorphic function to create more complex instantiation
map_pair : Pair(a, b), (a -> c), (b -> d) -> Pair(c, d)
map_pair = |(x, y), f, g| (f(x), g(y))

# This should trigger multiple instantiations
# First swap_pair gets instantiated, then map_pair
# The error should involve deeply nested instantiated types
main = {
    # This creates Pair(Num, Num)
    p1 = swap_pair((1, 2))

    # This should fail - map_pair expects a tuple but gets four separate arguments
    # And the instantiated types from map_pair should cause issues
    p2 = map_pair(3, 4, (|x| x + 1), (|y| y * 2))

    p2
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar OpenRound LowerIdent Comma LowerIdent CloseRound OpBar OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound Comma OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar OpenRound LowerIdent Comma LowerIdent CloseRound Comma LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpenRound LowerIdent CloseRound CloseRound BlankLine LineComment LineComment LineComment LowerIdent OpAssign OpenCurly LineComment LowerIdent OpAssign LowerIdent OpenRound OpenRound Int Comma Int CloseRound CloseRound BlankLine LineComment LineComment LowerIdent OpAssign LowerIdent OpenRound Int Comma Int Comma OpenRound OpBar LowerIdent OpBar LowerIdent OpPlus Int CloseRound Comma OpenRound OpBar LowerIdent OpBar LowerIdent OpStar Int CloseRound CloseRound BlankLine LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block)
      )
    )
))
(block
  (binop_colon
    (apply_uc
      (uc "Pair")
      (tuple_literal
        (lc "a")
        (lc "b")
      )
    )
    (tuple_literal
      (lc "a")
      (lc "b")
    )
  )
  (binop_colon
    (lc "swap_pair")
    (binop_arrow_call
      (apply_uc
        (uc "Pair")
        (tuple_literal
          (lc "a")
          (lc "b")
        )
      )
      (apply_uc
        (uc "Pair")
        (tuple_literal
          (lc "b")
          (lc "a")
        )
      )
    )
  )
  (binop_equals
    (lc "swap_pair")
    (lambda
      (body
        (tuple_literal
          (lc "y")
          (lc "x")
        )
      )
      (args
        (tuple_literal
          (lc "x")
          (lc "y")
        )
      )
    )
  )
  (binop_colon
    (lc "map_pair")
    (binop_arrow_call
      (apply_uc
        (uc "Pair")
        (tuple_literal
          (lc "a")
          (lc "b")
        )
      )
      (binop_arrow_call
        (binop_arrow_call
          (lc "a")
          (lc "c")
        )
        (binop_arrow_call
          (binop_arrow_call
            (lc "b")
            (lc "d")
          )
          (apply_uc
            (uc "Pair")
            (tuple_literal
              (lc "c")
              (lc "d")
            )
          )
        )
      )
    )
  )
  (binop_equals
    (lc "map_pair")
    (lambda
      (body
        (tuple_literal
          (apply_lc
            (lc "f")
            (lc "x")
          )
          (apply_lc
            (lc "g")
            (lc "y")
          )
        )
      )
      (args
        (tuple_literal
          (lc "x")
          (lc "y")
        )
        (lc "f")
        (lc "g")
      )
    )
  )
  (binop_equals
    (lc "main")
    (block
      (binop_equals
        (lc "p1")
        (apply_lc
          (lc "swap_pair")
          (tuple_literal
            (num_literal_i32 1)
            (num_literal_i32 2)
          )
        )
      )
      (binop_equals
        (lc "p2")
        (apply_lc
          (lc "map_pair")
          (tuple_literal
            (num_literal_i32 3)
            (num_literal_i32 4)
            (lambda
              (body
                (binop_plus
                  (lc "x")
                  (num_literal_i32 1)
                )
              )
              (args
                (lc "x")
              )
            )
            (lambda
              (body
                (binop_star
                  (lc "y")
                  (num_literal_i32 2)
                )
              )
              (args
                (lc "y")
              )
            )
          )
        )
      )
      (lc "p2")
    )
  )
)
~~~
# FORMATTED
~~~roc
app [main] { pf: "../basic-cli/platform.roc" platform [] }

# Type alias with parameters, just like the original
Pair((a, b)) : (a, b)
# Function that uses the alias and will need instantiation
swap_pair : Pair(a, b) -> Pair(b, a)
swap_pair = |x, y| (y, x)
# Another polymorphic function to create more complex instantiation
map_pair : Pair(a, b) -> (a -> c) -> (b -> d) -> Pair(c, d)
map_pair = |x, y, f, g| (f(x), g(y))
# This should trigger multiple instantiations
# First swap_pair gets instantiated, then map_pair
# The error should involve deeply nested instantiated types
main = {
	# This creates Pair(Num, Num)
	p1 = swap_pair((1, 2))
	# This should fail - map_pair expects a tuple but gets four separate arguments
	# And the instantiated types from map_pair should cause issues
	p2 = map_pair((3, 4, |x| x + 1, |y| y * 2))
	p2
}
~~~
# EXPECTED
UNUSED VARIABLE - test_exact_pattern_crash.md:19:5:19:7
TYPE MISMATCH - test_exact_pattern_crash.md:23:10:23:18
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "swap_pair"))
    (type type_27)
  )
  (Stmt.assign
    (pattern (Patt.ident "swap_pair"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "map_pair"))
    (type type_57)
  )
  (Stmt.assign
    (pattern (Patt.ident "map_pair"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.block
      (Stmt.assign
        (pattern (Patt.ident "p1"))
        (Expr.fn_call)
      )
      (Stmt.assign
        (pattern (Patt.ident "p2"))
        (Expr.fn_call)
      )
      (Expr.lookup "p2")
    )
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 121
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 -> #105)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 -> #104)
(var #36 -> #105)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 -> #112)
(var #60 _)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 -> #109)
(var #66 _)
(var #67 _)
(var #68 -> #110)
(var #69 _)
(var #70 _)
(var #71 -> #111)
(var #72 -> #112)
(var #73 _)
(var #74 -> #100)
(var #75 -> #80)
(var #76 -> #114)
(var #77 Num *)
(var #78 Num *)
(var #79 -> #113)
(var #80 _)
(var #81 _)
(var #82 -> #97)
(var #83 -> #120)
(var #84 Num *)
(var #85 Num *)
(var #86 _)
(var #87 -> #88)
(var #88 -> #89)
(var #89 Num *)
(var #90 -> #116)
(var #91 _)
(var #92 -> #93)
(var #93 -> #94)
(var #94 Num *)
(var #95 -> #118)
(var #96 -> #119)
(var #97 _)
(var #98 _)
(var #99 _)
(var #100 _)
(var #101 _)
(var #102 _)
(var #103 _)
(var #104 tuple)
(var #105 fn_pure)
(var #106 _)
(var #107 _)
(var #108 _)
(var #109 fn_pure)
(var #110 fn_pure)
(var #111 tuple)
(var #112 fn_pure)
(var #113 tuple)
(var #114 fn_pure)
(var #115 _)
(var #116 fn_pure)
(var #117 _)
(var #118 fn_pure)
(var #119 tuple)
(var #120 fn_pure)
~~~
# TYPES
~~~roc
x : _e
f : _e
y : _e
main : _e
swap_pair : _arg -> (_field, _field2)
p1 : _e
map_pair : _arg, _arg2, _arg3 -> (_field, _field2)
p2 : _e
g : _e
~~~
