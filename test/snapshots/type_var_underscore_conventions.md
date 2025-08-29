# META
~~~ini
description=Comprehensive test of type variable underscore conventions
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

# Test 1: UNUSED TYPE VARIABLE NAME - single-use variable should start with underscore
single_use : List(elem) -> Str
single_use = |x| "hello"

# Test 2: TYPE VAR ENDING IN UNDERSCORE - variables should never end with underscore
ending_underscore : List(elem_) -> elem_
ending_underscore = |list| "default"

# Test 3: COMBINATION - single-use ending in underscore (both errors)
combo_single : List(bad_) -> Str
combo_single = |x| "combo"

# Test 4: VALID CASES - these should not generate warnings
valid_single : List(_elem) -> Str
valid_single = |x| "valid"

valid_multi : elem -> List(elem)
valid_multi = |x| [x]

main = |x| "done"
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpColon LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenSquare LowerIdent CloseSquare LowerIdent OpAssign OpBar LowerIdent OpBar String ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "single_use")
    (binop_thin_arrow
      (apply_uc
        (uc "List")
        (lc "elem")
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "single_use")
    (lambda
      (body
        (str_literal_big "hello")
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_colon
    (lc "ending_underscore")
    (binop_thin_arrow
      (apply_uc
        (uc "List")
        (lc "elem_")
      )
      (lc "elem_")
    )
  )
  (binop_equals
    (lc "ending_underscore")
    (lambda
      (body
        (str_literal_big "default")
      )
      (args
        (lc "list")
      )
    )
  )
  (binop_colon
    (lc "combo_single")
    (binop_thin_arrow
      (apply_uc
        (uc "List")
        (lc "bad_")
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "combo_single")
    (lambda
      (body
        (str_literal_big "combo")
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_colon
    (lc "valid_single")
    (binop_thin_arrow
      (apply_uc
        (uc "List")
        (lc "_elem")
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "valid_single")
    (lambda
      (body
        (str_literal_big "valid")
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_colon
    (lc "valid_multi")
    (binop_thin_arrow
      (lc "elem")
      (apply_uc
        (uc "List")
        (lc "elem")
      )
    )
  )
  (binop_equals
    (lc "valid_multi")
    (lambda
      (body
        (list_literal
          (lc "x")
        )
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_equals
    (lc "main")
    (lambda
      (body
        (str_literal_small "done")
      )
      (args
        (lc "x")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main] }

single_use : List elem -> Str
single_use = |x| "hello"
ending_underscore : List elem_ -> elem_
ending_underscore = |list| "default"
combo_single : List bad_ -> Str
combo_single = |x| "combo"
valid_single : List _elem -> Str
valid_single = |x| "valid"
valid_multi : elem -> List elem
valid_multi = |x| [x]
main = |x| "done"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "single_use")
    (Expr.binop_thin_arrow)
  )
  (Expr.binop_equals
    (Expr.lookup "single_use")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "ending_underscore")
    (Expr.binop_thin_arrow)
  )
  (Expr.binop_equals
    (Expr.lookup "ending_underscore")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "combo_single")
    (Expr.binop_thin_arrow)
  )
  (Expr.binop_equals
    (Expr.lookup "combo_single")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "valid_single")
    (Expr.binop_thin_arrow)
  )
  (Expr.binop_equals
    (Expr.lookup "valid_single")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "valid_multi")
    (Expr.binop_thin_arrow)
  )
  (Expr.binop_equals
    (Expr.lookup "valid_multi")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "main")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
single_use : _a
ending_underscore : _a
combo_single : _a
valid_single : _a
valid_multi : _a
main : _a
~~~
