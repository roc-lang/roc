# META
~~~ini
description=Simple unused and used underscore variable test
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Regular variable that is unused - should warn
unused_regular = |x| 42

# Underscore variable that is used - should warn
used_underscore = |_value| _value

# Underscore variable that is unused - should be fine
unused_underscore = |_ignored| 100

# Regular variable that is used - should be fine
used_regular = |number| number + 1

main! = |_| {
    a = unused_regular(5)
    b = used_underscore(10)
    c = unused_underscore(15)
    d = used_regular(20)
    a + b + c + d
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar Int BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar Int BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Int BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block)
      )
    )
))
(block
  (binop_equals
    (lc "unused_regular")
    (lambda
      (body
        (num_literal_i32 42)
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_equals
    (lc "used_underscore")
    (lambda
      (body
        (lc "_value")
      )
      (args
        (lc "_value")
      )
    )
  )
  (binop_equals
    (lc "unused_underscore")
    (lambda
      (body
        (num_literal_i32 100)
      )
      (args
        (lc "_ignored")
      )
    )
  )
  (binop_equals
    (lc "used_regular")
    (lambda
      (body
        (binop_plus
          (lc "number")
          (num_literal_i32 1)
        )
      )
      (args
        (lc "number")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (block
          (binop_equals
            (lc "a")
            (apply_lc
              (lc "unused_regular")
              (num_literal_i32 5)
            )
          )
          (binop_equals
            (lc "b")
            (apply_lc
              (lc "used_underscore")
              (num_literal_i32 10)
            )
          )
          (binop_equals
            (lc "c")
            (apply_lc
              (lc "unused_underscore")
              (num_literal_i32 15)
            )
          )
          (binop_equals
            (lc "d")
            (apply_lc
              (lc "used_regular")
              (num_literal_i32 20)
            )
          )
          (binop_plus
            (binop_plus
              (binop_plus
                (lc "a")
                (lc "b")
              )
              (lc "c")
            )
            (lc "d")
          )
        )
      )
      (args
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app [main!] { pf: "../basic-cli/main.roc" platform [] }

# Regular variable that is unused - should warn
unused_regular = |x| 42
# Underscore variable that is used - should warn
used_underscore = |_value| _value
# Underscore variable that is unused - should be fine
unused_underscore = |_ignored| 100
# Regular variable that is used - should be fine
used_regular = |number| number + 1
main! = |_| {
	a = unused_regular(5)
	b = used_underscore(10)
	c = unused_underscore(15)
	d = used_regular(20)
	((a + b) + c) + d
}
~~~
# EXPECTED
UNUSED VARIABLE - unused_vars_simple.md:4:19:4:20
UNDERSCORE VARIABLE USED - unused_vars_simple.md:7:28:7:34
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "unused_regular"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "used_underscore"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "unused_underscore"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "used_regular"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 76
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 -> #63)
(var #8 _)
(var #9 Num *)
(var #10 -> #63)
(var #11 _)
(var #12 -> #65)
(var #13 _)
(var #14 _)
(var #15 -> #65)
(var #16 _)
(var #17 -> #67)
(var #18 _)
(var #19 Num *)
(var #20 -> #67)
(var #21 _)
(var #22 -> #69)
(var #23 _)
(var #24 -> #25)
(var #25 -> #26)
(var #26 Num *)
(var #27 -> #69)
(var #28 _)
(var #29 -> #75)
(var #30 _)
(var #31 -> #34)
(var #32 -> #71)
(var #33 Num *)
(var #34 _)
(var #35 _)
(var #36 -> #39)
(var #37 -> #72)
(var #38 Num *)
(var #39 _)
(var #40 _)
(var #41 -> #44)
(var #42 -> #73)
(var #43 Num *)
(var #44 _)
(var #45 _)
(var #46 -> #49)
(var #47 -> #74)
(var #48 Num *)
(var #49 _)
(var #50 _)
(var #51 -> #52)
(var #52 -> #53)
(var #53 -> #54)
(var #54 -> #55)
(var #55 -> #56)
(var #56 -> #57)
(var #57 _)
(var #58 _)
(var #59 -> #75)
(var #60 _)
(var #61 _)
(var #62 _)
(var #63 fn_pure)
(var #64 _)
(var #65 fn_pure)
(var #66 _)
(var #67 fn_pure)
(var #68 _)
(var #69 fn_pure)
(var #70 _)
(var #71 fn_pure)
(var #72 fn_pure)
(var #73 fn_pure)
(var #74 fn_pure)
(var #75 fn_pure)
~~~
# TYPES
~~~roc
_ignored : _e
c : _e
d : _e
a : _e
unused_regular : _arg -> Num(_size)
_value : _e
used_regular : _arg -> Num(_size)
b : _e
x : _e
number : _e
used_underscore : _arg -> _ret
main : _arg -> _ret
unused_underscore : _arg -> Num(_size)
~~~
