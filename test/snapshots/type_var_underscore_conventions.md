# META
~~~ini
description=Comprehensive test of type variable underscore conventions
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

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
KwApp OpenSquare LowerIdent CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpColon LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenSquare LowerIdent CloseSquare LowerIdent OpAssign OpBar LowerIdent OpBar String ~~~
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
NO CHANGE
~~~
# EXPECTED
UNUSED VARIABLE - type_var_underscore_conventions.md:5:15:5:16
UNUSED VARIABLE - type_var_underscore_conventions.md:9:22:9:26
UNUSED VARIABLE - type_var_underscore_conventions.md:13:17:13:18
UNUSED VARIABLE - type_var_underscore_conventions.md:17:17:17:18
UNUSED VARIABLE - type_var_underscore_conventions.md:22:9:22:10
TYPE MISMATCH - type_var_underscore_conventions.md:8:36:8:41
# PROBLEMS
**Unsupported Node**
at 4:14 to 4:31

**Unsupported Node**
at 5:14 to 5:18

**Unsupported Node**
at 8:21 to 8:41

**Unsupported Node**
at 9:21 to 9:28

**Unsupported Node**
at 12:16 to 12:33

**Unsupported Node**
at 13:16 to 13:20

**Unsupported Node**
at 16:16 to 16:34

**Unsupported Node**
at 17:16 to 17:20

**Unsupported Node**
at 19:15 to 19:33

**Unsupported Node**
at 20:15 to 20:19

**Unsupported Node**
at 22:8 to 22:12

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "single_use")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "ending_underscore")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "combo_single")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "valid_single")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "valid_multi")
    (Expr.malformed)
  )
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
~~~
