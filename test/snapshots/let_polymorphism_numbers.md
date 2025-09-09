# META
~~~ini
description=Let-polymorphism with numbers
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic number polymorphism
num = 42
frac = 4.2

# Using polymorphic values in different contexts
int_use = num
float_use = frac

# num used as Int
int_add = num + 10
int_multiply = num * 2

# num used as Float
float_add = num + 3.14
float_multiply = num * 2.5

# Polymorphic function with numeric types
double = |x| x * 2

# Used with different numeric types
int_doubled = double(5)
float_doubled = double(2.5)

main = |_| {
    # Combine results
    int_add + int_multiply
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpAssign Int LowerIdent OpAssign Float BlankLine LineComment LowerIdent OpAssign LowerIdent LowerIdent OpAssign LowerIdent BlankLine LineComment LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent OpAssign LowerIdent OpStar Int BlankLine LineComment LowerIdent OpAssign LowerIdent OpPlus Float LowerIdent OpAssign LowerIdent OpStar Float BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpStar Int BlankLine LineComment LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Float CloseRound BlankLine LowerIdent OpAssign OpBar Underscore OpBar OpenCurly LineComment LowerIdent OpPlus LowerIdent CloseCurly ~~~
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
  (binop_equals
    (lc "num")
    (num_literal_i32 42)
  )
  (binop_equals
    (lc "frac")
    (frac_literal_small 4.2)
  )
  (binop_equals
    (lc "int_use")
    (lc "num")
  )
  (binop_equals
    (lc "float_use")
    (lc "frac")
  )
  (binop_equals
    (lc "int_add")
    (binop_plus
      (lc "num")
      (num_literal_i32 10)
    )
  )
  (binop_equals
    (lc "int_multiply")
    (binop_star
      (lc "num")
      (num_literal_i32 2)
    )
  )
  (binop_equals
    (lc "float_add")
    (binop_plus
      (lc "num")
      (frac_literal_small 3.14)
    )
  )
  (binop_equals
    (lc "float_multiply")
    (binop_star
      (lc "num")
      (frac_literal_small 2.5)
    )
  )
  (binop_equals
    (lc "double")
    (lambda
      (body
        (binop_star
          (lc "x")
          (num_literal_i32 2)
        )
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_equals
    (lc "int_doubled")
    (apply_lc
      (lc "double")
      (num_literal_i32 5)
    )
  )
  (binop_equals
    (lc "float_doubled")
    (apply_lc
      (lc "double")
      (frac_literal_small 2.5)
    )
  )
  (binop_equals
    (lc "main")
    (lambda
      (body
        (block
          (binop_plus
            (lc "int_add")
            (lc "int_multiply")
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
app [main] { pf: "../basic-cli/platform.roc" platform [] }

# Basic number polymorphism
num = 42
frac = 4.2
# Using polymorphic values in different contexts
int_use = num
float_use = frac
# num used as Int
int_add = num + 10
int_multiply = num * 2
# num used as Float
float_add = num + 3.14
float_multiply = num * 2.5
# Polymorphic function with numeric types
double = |x| x * 2
# Used with different numeric types
int_doubled = double(5)
float_doubled = double(2.5)
main = |_| {
	# Combine results
	int_add + int_multiply
}
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_numbers.md:4:1:4:4:**
```roc
num = 42
```
^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_numbers.md:5:1:5:5:**
```roc
frac = 4.2
```
^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_numbers.md:8:1:8:8:**
```roc
int_use = num
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_numbers.md:9:1:9:10:**
```roc
float_use = frac
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_numbers.md:12:1:12:8:**
```roc
int_add = num + 10
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_numbers.md:13:1:13:13:**
```roc
int_multiply = num * 2
```
^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_numbers.md:16:1:16:10:**
```roc
float_add = num + 3.14
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_numbers.md:17:1:17:15:**
```roc
float_multiply = num * 2.5
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_numbers.md:20:1:20:7:**
```roc
double = |x| x * 2
```
^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_numbers.md:23:1:23:12:**
```roc
int_doubled = double(5)
```
^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**let_polymorphism_numbers.md:24:1:24:14:**
```roc
float_doubled = double(2.5)
```
^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "num"))
    (Expr.num_literal_i32 42)
  )
  (Stmt.assign
    (pattern (Patt.ident "frac"))
    (Expr.frac_literal_small 4.2)
  )
  (Stmt.assign
    (pattern (Patt.ident "int_use"))
    (Expr.lookup "num")
  )
  (Stmt.assign
    (pattern (Patt.ident "float_use"))
    (Expr.lookup "frac")
  )
  (Stmt.assign
    (pattern (Patt.ident "int_add"))
    (Expr.binop_plus
      (Expr.lookup "num")
      (Expr.num_literal_i32 10)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "int_multiply"))
    (Expr.binop_star
      (Expr.lookup "num")
      (Expr.num_literal_i32 2)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "float_add"))
    (Expr.binop_plus
      (Expr.lookup "num")
      (Expr.frac_literal_small 3.14)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "float_multiply"))
    (Expr.binop_star
      (Expr.lookup "num")
      (Expr.frac_literal_small 2.5)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "double"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "int_doubled"))
    (Expr.fn_call)
  )
  (Stmt.assign
    (pattern (Patt.ident "float_doubled"))
    (Expr.fn_call)
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 71
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 -> #8)
(var #8 Num *)
(var #9 _)
(var #10 -> #11)
(var #11 F64)
(var #12 _)
(var #13 -> #14)
(var #14 _)
(var #15 _)
(var #16 -> #17)
(var #17 _)
(var #18 _)
(var #19 -> #22)
(var #20 -> #21)
(var #21 -> #22)
(var #22 Num *)
(var #23 _)
(var #24 -> #27)
(var #25 -> #26)
(var #26 -> #27)
(var #27 Num *)
(var #28 _)
(var #29 -> #32)
(var #30 -> #31)
(var #31 -> #32)
(var #32 F64)
(var #33 _)
(var #34 -> #37)
(var #35 -> #36)
(var #36 -> #37)
(var #37 F64)
(var #38 _)
(var #39 -> #66)
(var #40 _)
(var #41 -> #42)
(var #42 -> #43)
(var #43 Num *)
(var #44 -> #66)
(var #45 _)
(var #46 -> #49)
(var #47 -> #67)
(var #48 Num *)
(var #49 _)
(var #50 _)
(var #51 -> #54)
(var #52 -> #68)
(var #53 F64)
(var #54 _)
(var #55 _)
(var #56 -> #70)
(var #57 _)
(var #58 -> #59)
(var #59 -> #60)
(var #60 _)
(var #61 _)
(var #62 -> #70)
(var #63 _)
(var #64 _)
(var #65 _)
(var #66 fn_pure)
(var #67 fn_pure)
(var #68 fn_pure)
(var #69 _)
(var #70 fn_pure)
~~~
# TYPES
~~~roc
float_add : F64
num : Num(_size)
double : _arg -> Num(_size)
float_multiply : F64
int_doubled : _a
int_multiply : Num(_size)
int_add : Num(_size)
frac : F64
x : _a
float_doubled : _a
float_use : _a
int_use : _a
~~~
