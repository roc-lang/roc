# META
~~~ini
description=Simple demonstration that type variable names avoid collision with existing identifiers
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Define some variables that would normally be used for type variables
a = 1
b = 2
c = 3

# This identity function should get type 'd -> d' since a, b, c are taken
identity = |x| x

# This function should get type 'e -> e' since d is now also taken
identity2 = |y| y

# This function with two parameters should get types 'f, g -> (f, g)'
pair = |first, second| (first, second)

main! = |_| {
    result1 = identity(42)
    result2 = identity2("hello")
    result3 = pair(result1, result2)
    
    a + b + c
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpAssign Int LowerIdent OpAssign Int LowerIdent OpAssign Int BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LineComment LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound String CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseCurly ~~~
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
    (lc "a")
    (num_literal_i32 1)
  )
  (binop_equals
    (lc "b")
    (num_literal_i32 2)
  )
  (binop_equals
    (lc "c")
    (num_literal_i32 3)
  )
  (binop_equals
    (lc "identity")
    (lambda
      (body
        (lc "x")
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_equals
    (lc "identity2")
    (lambda
      (body
        (lc "y")
      )
      (args
        (lc "y")
      )
    )
  )
  (binop_equals
    (lc "pair")
    (lambda
      (body
        (tuple_literal
          (lc "first")
          (lc "second")
        )
      )
      (args
        (lc "first")
        (lc "second")
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
              (lc "identity")
              (num_literal_i32 42)
            )
          )
          (binop_equals
            (lc "result2")
            (apply_lc
              (lc "identity2")
              (str_literal_big "hello")
            )
          )
          (binop_equals
            (lc "result3")
            (apply_lc
              (lc "pair")
              (tuple_literal
                (lc "result1")
                (lc "result2")
              )
            )
          )
          (binop_plus
            (binop_plus
              (lc "a")
              (lc "b")
            )
            (lc "c")
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

# Define some variables that would normally be used for type variables
a = 1
b = 2
c = 3
# This identity function should get type 'd -> d' since a, b, c are taken
identity = |x| x
# This function should get type 'e -> e' since d is now also taken
identity2 = |y| y
# This function with two parameters should get types 'f, g -> (f, g)'
pair = |first, second| (first, second)
main! = |_| {
	result1 = identity(42)
	result2 = identity2("hello")
	result3 = pair((result1, result2))
	(a + b) + c
}
~~~
# EXPECTED
UNUSED VARIABLE - type_var_collision_simple.md:20:5:20:12
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_var_collision_simple.md:4:1:4:2:**
```roc
a = 1
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_collision_simple.md:5:1:5:2:**
```roc
b = 2
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_collision_simple.md:6:1:6:2:**
```roc
c = 3
```
^


**SHADOWING**
This definition shadows an existing one.

**type_var_collision_simple.md:9:1:9:9:**
```roc
identity = |x| x
```
^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_collision_simple.md:12:1:12:10:**
```roc
identity2 = |y| y
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_var_collision_simple.md:15:1:15:5:**
```roc
pair = |first, second| (first, second)
```
^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "a"))
    (Expr.num_literal_i32 1)
  )
  (Stmt.assign
    (pattern (Patt.ident "b"))
    (Expr.num_literal_i32 2)
  )
  (Stmt.assign
    (pattern (Patt.ident "c"))
    (Expr.num_literal_i32 3)
  )
  (Stmt.assign
    (pattern (Patt.ident "identity"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "identity2"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "pair"))
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
(var #7 -> #8)
(var #8 Num *)
(var #9 _)
(var #10 -> #11)
(var #11 Num *)
(var #12 _)
(var #13 -> #14)
(var #14 Num *)
(var #15 _)
(var #16 -> #63)
(var #17 _)
(var #18 _)
(var #19 -> #63)
(var #20 _)
(var #21 -> #65)
(var #22 _)
(var #23 _)
(var #24 -> #65)
(var #25 _)
(var #26 -> #69)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 -> #68)
(var #32 -> #69)
(var #33 _)
(var #34 -> #75)
(var #35 _)
(var #36 -> #39)
(var #37 -> #71)
(var #38 Num *)
(var #39 _)
(var #40 _)
(var #41 -> #44)
(var #42 -> #72)
(var #43 Str)
(var #44 _)
(var #45 _)
(var #46 -> #51)
(var #47 -> #74)
(var #48 _)
(var #49 _)
(var #50 -> #73)
(var #51 _)
(var #52 _)
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
(var #67 _)
(var #68 tuple)
(var #69 fn_pure)
(var #70 _)
(var #71 fn_pure)
(var #72 fn_pure)
(var #73 tuple)
(var #74 fn_pure)
(var #75 fn_pure)
~~~
# TYPES
~~~roc
a : Num(_size)
first : _d
b : Num(_size)
identity : _arg -> _ret
c : Num(_size)
x : _d
y : _d
pair : _arg, _arg2 -> (_field, _field2)
identity2 : _arg -> _ret
main : _arg -> _ret
second : _d
~~~
