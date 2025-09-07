# META
~~~ini
description=Lambda parameters with unused variable checking
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Lambda with unused parameter - should warn
add : U64 -> U64
add = |unused| 42

# Lambda with underscore parameter that is used - should warn
multiply : U64 -> U64
multiply = |_factor| _factor * 2

# Lambda with unused underscore parameter - should be fine
process : U64 -> U64
process = |_input| 100

# Lambda with used parameter - should be fine
double : U64 -> U64
double = |value| value * 2

main! = |_| {
    result1 = add(5)
    result2 = multiply(3)
    result3 = process(7)
    result4 = double(4)
    result1 + result2 + result3 + result4
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpStar Int BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar Int BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpStar Int BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseCurly ~~~
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
  (binop_colon
    (lc "add")
    (binop_arrow_call
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "add")
    (lambda
      (body
        (num_literal_i32 42)
      )
      (args
        (lc "unused")
      )
    )
  )
  (binop_colon
    (lc "multiply")
    (binop_arrow_call
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "multiply")
    (lambda
      (body
        (binop_star
          (lc "_factor")
          (num_literal_i32 2)
        )
      )
      (args
        (lc "_factor")
      )
    )
  )
  (binop_colon
    (lc "process")
    (binop_arrow_call
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "process")
    (lambda
      (body
        (num_literal_i32 100)
      )
      (args
        (lc "_input")
      )
    )
  )
  (binop_colon
    (lc "double")
    (binop_arrow_call
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "double")
    (lambda
      (body
        (binop_star
          (lc "value")
          (num_literal_i32 2)
        )
      )
      (args
        (lc "value")
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (block
          (binop_equals
            (lc "result1")
            (apply_lc
              (lc "add")
              (num_literal_i32 5)
            )
          )
          (binop_equals
            (lc "result2")
            (apply_lc
              (lc "multiply")
              (num_literal_i32 3)
            )
          )
          (binop_equals
            (lc "result3")
            (apply_lc
              (lc "process")
              (num_literal_i32 7)
            )
          )
          (binop_equals
            (lc "result4")
            (apply_lc
              (lc "double")
              (num_literal_i32 4)
            )
          )
          (binop_plus
            (binop_plus
              (binop_plus
                (lc "result1")
                (lc "result2")
              )
              (lc "result3")
            )
            (lc "result4")
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

# Lambda with unused parameter - should warn
add : U64 -> U64
add = |unused| 42
# Lambda with underscore parameter that is used - should warn
multiply : U64 -> U64
multiply = |_factor| _factor * 2
# Lambda with unused underscore parameter - should be fine
process : U64 -> U64
process = |_input| 100
# Lambda with used parameter - should be fine
double : U64 -> U64
double = |value| value * 2
main! = |_| {
	result1 = add(5)
	result2 = multiply(3)
	result3 = process(7)
	result4 = double(4)
	((result1 + result2) + result3) + result4
}
~~~
# EXPECTED
UNUSED VARIABLE - lambda_parameter_unused.md:5:8:5:14
UNDERSCORE VARIABLE USED - lambda_parameter_unused.md:9:22:9:29
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**lambda_parameter_unused.md:4:1:4:4:**
```roc
add : U64 -> U64
```
^^^


**SHADOWING**
This definition shadows an existing one.

**lambda_parameter_unused.md:5:1:5:4:**
```roc
add = |unused| 42
```
^^^


**SHADOWING**
This definition shadows an existing one.

**lambda_parameter_unused.md:8:1:8:9:**
```roc
multiply : U64 -> U64
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**lambda_parameter_unused.md:9:1:9:9:**
```roc
multiply = |_factor| _factor * 2
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**lambda_parameter_unused.md:12:1:12:8:**
```roc
process : U64 -> U64
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**lambda_parameter_unused.md:13:1:13:8:**
```roc
process = |_input| 100
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**lambda_parameter_unused.md:16:1:16:7:**
```roc
double : U64 -> U64
```
^^^^^^


**SHADOWING**
This definition shadows an existing one.

**lambda_parameter_unused.md:17:1:17:7:**
```roc
double = |value| value * 2
```
^^^^^^


**SHADOWING**
This definition shadows an existing one.

**lambda_parameter_unused.md:19:1:19:6:**
```roc
main! = |_| {
```
^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "add"))
    (type type_10)
  )
  (Stmt.assign
    (pattern (Patt.ident "add"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "multiply"))
    (type type_20)
  )
  (Stmt.assign
    (pattern (Patt.ident "multiply"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "process"))
    (type type_32)
  )
  (Stmt.assign
    (pattern (Patt.ident "process"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "double"))
    (type type_42)
  )
  (Stmt.assign
    (pattern (Patt.ident "double"))
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
; Total type variables: 98
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
(var #12 -> #85)
(var #13 _)
(var #14 Num *)
(var #15 -> #85)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 -> #87)
(var #23 _)
(var #24 -> #25)
(var #25 -> #26)
(var #26 Num *)
(var #27 -> #87)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 -> #89)
(var #35 _)
(var #36 Num *)
(var #37 -> #89)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 -> #91)
(var #45 _)
(var #46 -> #47)
(var #47 -> #48)
(var #48 Num *)
(var #49 -> #91)
(var #50 _)
(var #51 -> #97)
(var #52 _)
(var #53 -> #56)
(var #54 -> #93)
(var #55 Num *)
(var #56 _)
(var #57 _)
(var #58 -> #61)
(var #59 -> #94)
(var #60 Num *)
(var #61 _)
(var #62 _)
(var #63 -> #66)
(var #64 -> #95)
(var #65 Num *)
(var #66 _)
(var #67 _)
(var #68 -> #71)
(var #69 -> #96)
(var #70 Num *)
(var #71 _)
(var #72 _)
(var #73 -> #74)
(var #74 -> #75)
(var #75 -> #76)
(var #76 -> #77)
(var #77 -> #78)
(var #78 -> #79)
(var #79 _)
(var #80 _)
(var #81 -> #97)
(var #82 _)
(var #83 _)
(var #84 _)
(var #85 fn_pure)
(var #86 _)
(var #87 fn_pure)
(var #88 _)
(var #89 fn_pure)
(var #90 _)
(var #91 fn_pure)
(var #92 _)
(var #93 fn_pure)
(var #94 fn_pure)
(var #95 fn_pure)
(var #96 fn_pure)
(var #97 fn_pure)
~~~
# TYPES
~~~roc
value : _a
process : _arg -> Num(_size)
add : _arg -> Num(_size)
_factor : _a
multiply : _arg -> Num(_size)
unused : _a
_input : _a
double : _arg -> Num(_size)
main : _arg -> _ret
~~~
