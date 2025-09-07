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
KwApp OpenSquare LowerIdent CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LowerIdent OpColon LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar String ~~~
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
    (lc "single_use")
    (binop_arrow_call
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
    (binop_arrow_call
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
    (binop_arrow_call
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
    (binop_arrow_call
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
    (binop_arrow_call
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
app [main] { pf: "../basic-cli/platform.roc" platform [] }

# Test 1: UNUSED TYPE VARIABLE NAME - single-use variable should start with underscore
single_use : List elem -> Str
single_use = |x| "hello"
# Test 2: TYPE VAR ENDING IN UNDERSCORE - variables should never end with underscore
ending_underscore : List elem_ -> elem_
ending_underscore = |list| "default"
# Test 3: COMBINATION - single-use ending in underscore (both errors)
combo_single : List bad_ -> Str
combo_single = |x| "combo"
# Test 4: VALID CASES - these should not generate warnings
valid_single : List _elem -> Str
valid_single = |x| "valid"
valid_multi : elem -> List elem
valid_multi = |x| [x]
main = |x| "done"
~~~
# EXPECTED
UNUSED VARIABLE - type_var_underscore_conventions.md:5:15:5:16
UNUSED VARIABLE - type_var_underscore_conventions.md:9:22:9:26
UNUSED VARIABLE - type_var_underscore_conventions.md:13:17:13:18
UNUSED VARIABLE - type_var_underscore_conventions.md:17:17:17:18
UNUSED VARIABLE - type_var_underscore_conventions.md:22:9:22:10
TYPE MISMATCH - type_var_underscore_conventions.md:9:28:9:37
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_var_underscore_conventions.md:4:1:4:11:**
```roc
single_use : List(elem) -> Str
```
^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_underscore_conventions.md:5:1:5:11:**
```roc
single_use = |x| "hello"
```
^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_underscore_conventions.md:8:1:8:18:**
```roc
ending_underscore : List(elem_) -> elem_
```
^^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_underscore_conventions.md:9:1:9:18:**
```roc
ending_underscore = |list| "default"
```
^^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_underscore_conventions.md:12:1:12:13:**
```roc
combo_single : List(bad_) -> Str
```
^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_underscore_conventions.md:13:17:13:18:**
```roc
combo_single = |x| "combo"
```
                ^


**SHADOWING**
This definition shadows an existing one.

**type_var_underscore_conventions.md:13:1:13:13:**
```roc
combo_single = |x| "combo"
```
^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_underscore_conventions.md:16:1:16:13:**
```roc
valid_single : List(_elem) -> Str
```
^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_underscore_conventions.md:17:17:17:18:**
```roc
valid_single = |x| "valid"
```
                ^


**SHADOWING**
This definition shadows an existing one.

**type_var_underscore_conventions.md:17:1:17:13:**
```roc
valid_single = |x| "valid"
```
^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_underscore_conventions.md:19:1:19:12:**
```roc
valid_multi : elem -> List(elem)
```
^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_underscore_conventions.md:20:16:20:17:**
```roc
valid_multi = |x| [x]
```
               ^


**SHADOWING**
This definition shadows an existing one.

**type_var_underscore_conventions.md:20:1:20:12:**
```roc
valid_multi = |x| [x]
```
^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_underscore_conventions.md:22:9:22:10:**
```roc
main = |x| "done"
```
        ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "single_use"))
    (type type_12)
  )
  (Stmt.assign
    (pattern (Patt.ident "single_use"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "ending_underscore"))
    (type type_24)
  )
  (Stmt.assign
    (pattern (Patt.ident "ending_underscore"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "combo_single"))
    (type type_36)
  )
  (Stmt.assign
    (pattern (Patt.ident "combo_single"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "valid_single"))
    (type type_48)
  )
  (Stmt.assign
    (pattern (Patt.ident "valid_single"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "valid_multi"))
    (type type_60)
  )
  (Stmt.assign
    (pattern (Patt.ident "valid_multi"))
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
; Total type variables: 86
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
(var #14 -> #75)
(var #15 _)
(var #16 Str)
(var #17 -> #75)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 -> #77)
(var #27 _)
(var #28 Str)
(var #29 -> #77)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 -> #79)
(var #39 _)
(var #40 Str)
(var #41 -> #79)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 -> #81)
(var #51 _)
(var #52 Str)
(var #53 -> #81)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 _)
(var #61 _)
(var #62 -> #83)
(var #63 _)
(var #64 _)
(var #65 _)
(var #66 -> #83)
(var #67 _)
(var #68 -> #85)
(var #69 _)
(var #70 Str)
(var #71 -> #85)
(var #72 _)
(var #73 _)
(var #74 _)
(var #75 fn_pure)
(var #76 _)
(var #77 fn_pure)
(var #78 _)
(var #79 fn_pure)
(var #80 _)
(var #81 fn_pure)
(var #82 _)
(var #83 fn_pure)
(var #84 _)
(var #85 fn_pure)
~~~
# TYPES
~~~roc
list : _a
ending_underscore : _arg -> Str
valid_multi : _arg -> _ret
valid_single : _arg -> Str
x : _a
single_use : _arg -> Str
combo_single : _arg -> Str
~~~
