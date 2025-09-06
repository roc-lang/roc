# META
~~~ini
description=Polymorphic tuple function with instantiation crash
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# A polymorphic function that expects a tuple
swap : (a, b) -> (b, a)
swap = |(x, y)| (y, x)

# Call it with two separate arguments instead of a tuple
# This should trigger instantiation and then crash on error reporting
main = swap(1, 2)
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar OpenRound LowerIdent Comma LowerIdent CloseRound OpBar OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LineComment LowerIdent OpAssign LowerIdent OpenRound Int Comma Int CloseRound ~~~
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
    (lc "swap")
    (binop_arrow_call
      (tuple_literal
        (lc "a")
        (lc "b")
      )
      (tuple_literal
        (lc "b")
        (lc "a")
      )
    )
  )
  (binop_equals
    (lc "swap")
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
  (binop_equals
    (lc "main")
    (apply_lc
      (lc "swap")
      (tuple_literal
        (num_literal_i32 1)
        (num_literal_i32 2)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app [main] { pf: "../basic-cli/platform.roc" platform [] }

# A polymorphic function that expects a tuple
swap : (a, b) -> (b, a)
swap = |x, y| (y, x)
# Call it with two separate arguments instead of a tuple
# This should trigger instantiation and then crash on error reporting
main = swap((1, 2))
~~~
# EXPECTED
TYPE MISMATCH - test_tuple_instantiation_crash.md:9:8:9:12
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "swap"))
    (type type_14)
  )
  (Stmt.assign
    (pattern (Patt.ident "swap"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.fn_call)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 38
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
(var #16 -> #35)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 -> #34)
(var #23 -> #35)
(var #24 _)
(var #25 -> #30)
(var #26 -> #37)
(var #27 Num *)
(var #28 Num *)
(var #29 -> #36)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 tuple)
(var #35 fn_pure)
(var #36 tuple)
(var #37 fn_pure)
~~~
# TYPES
~~~roc
swap : _arg -> (_field, _field2)
main : _c
x : _c
y : _c
~~~
