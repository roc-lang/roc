# META
~~~ini
description=Lambda currying with polymorphic function constraints - tests if numeric literals in curried functions get properly constrained
type=file
~~~
# SOURCE
~~~roc
module [makeAdder, curriedAdd, applyTwice]

# Function that returns a function with polymorphic type
makeAdder : a -> (a -> a)
makeAdder = |x| |y| x + y

# Should constrain the literal 5 to I64
curriedAdd : I64 -> I64
curriedAdd = makeAdder(5)

# Higher-order function that applies a function twice
applyTwice : (a -> a), a -> a
applyTwice = |f, x| f(f(x))

# Should constrain the literal 3 to I64
addThreeTwice : I64 -> I64
addThreeTwice = |n| applyTwice(|x| x + 3, n)
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow OpenRound LowerIdent OpArrow LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign LowerIdent OpenRound Int CloseRound BlankLine LineComment LowerIdent OpColon OpenRound LowerIdent OpArrow LowerIdent CloseRound Comma LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent OpenRound LowerIdent OpenRound LowerIdent CloseRound CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpenRound OpBar LowerIdent OpBar LowerIdent OpPlus Int Comma LowerIdent CloseRound ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "makeAdder")

    (lc "curriedAdd")

    (lc "applyTwice")
))
(block
  (binop_colon
    (lc "makeAdder")
    (binop_arrow_call
      (lc "a")
      (binop_arrow_call
        (lc "a")
        (lc "a")
      )
    )
  )
  (binop_equals
    (lc "makeAdder")
    (lambda
      (body
        (lambda
          (body
            (binop_plus
              (lc "x")
              (lc "y")
            )
          )
          (args
            (lc "y")
          )
        )
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_colon
    (lc "curriedAdd")
    (binop_arrow_call
      (uc "I64")
      (uc "I64")
    )
  )
  (binop_equals
    (lc "curriedAdd")
    (apply_lc
      (lc "makeAdder")
      (num_literal_i32 5)
    )
  )
  (binop_colon
    (lc "applyTwice")
    (binop_arrow_call
      (binop_arrow_call
        (lc "a")
        (lc "a")
      )
      (binop_arrow_call
        (lc "a")
        (lc "a")
      )
    )
  )
  (binop_equals
    (lc "applyTwice")
    (lambda
      (body
        (apply_lc
          (lc "f")
          (apply_lc
            (lc "f")
            (lc "x")
          )
        )
      )
      (args
        (lc "f")
        (lc "x")
      )
    )
  )
  (binop_colon
    (lc "addThreeTwice")
    (binop_arrow_call
      (uc "I64")
      (uc "I64")
    )
  )
  (binop_equals
    (lc "addThreeTwice")
    (lambda
      (body
        (apply_lc
          (lc "applyTwice")
          (lambda
            (body
              (tuple_literal
                (binop_plus
                  (lc "x")
                  (num_literal_i32 3)
                )
                (lc "n")
              )
            )
            (args
              (lc "x")
            )
          )
        )
      )
      (args
        (lc "n")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [makeAdder, curriedAdd, applyTwice]

# Function that returns a function with polymorphic type
makeAdder : a -> a -> a
makeAdder = |x| |y| x + y
# Should constrain the literal 5 to I64
curriedAdd : I64 -> I64
curriedAdd = makeAdder(5)
# Higher-order function that applies a function twice
applyTwice :
	(a -> a) -> a -> a
applyTwice = |f, x| f(f(x))
# Should constrain the literal 3 to I64
addThreeTwice : I64 -> I64
addThreeTwice = |n| applyTwice(|x| (x + 3, n))
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**lambda_currying_constraint.md:5:1:5:10:**
```roc
makeAdder = |x| |y| x + y
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**lambda_currying_constraint.md:9:1:9:11:**
```roc
curriedAdd = makeAdder(5)
```
^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**lambda_currying_constraint.md:13:18:13:19:**
```roc
applyTwice = |f, x| f(f(x))
```
                 ^


**SHADOWING**
This definition shadows an existing one.

**lambda_currying_constraint.md:13:1:13:11:**
```roc
applyTwice = |f, x| f(f(x))
```
^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**lambda_currying_constraint.md:16:1:16:14:**
```roc
addThreeTwice : I64 -> I64
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**lambda_currying_constraint.md:17:33:17:34:**
```roc
addThreeTwice = |n| applyTwice(|x| x + 3, n)
```
                                ^


**SHADOWING**
This definition shadows an existing one.

**lambda_currying_constraint.md:17:1:17:14:**
```roc
addThreeTwice = |n| applyTwice(|x| x + 3, n)
```
^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "makeAdder"))
    (type type_9)
  )
  (Stmt.assign
    (pattern (Patt.ident "makeAdder"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "curriedAdd"))
    (type type_23)
  )
  (Stmt.assign
    (pattern (Patt.ident "curriedAdd"))
    (Expr.fn_call)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "applyTwice"))
    (type type_37)
  )
  (Stmt.assign
    (pattern (Patt.ident "applyTwice"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "addThreeTwice"))
    (type type_52)
  )
  (Stmt.assign
    (pattern (Patt.ident "addThreeTwice"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 85
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
(var #11 -> #71)
(var #12 _)
(var #13 _)
(var #14 -> #15)
(var #15 -> #16)
(var #16 _)
(var #17 -> #70)
(var #18 -> #71)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 -> #28)
(var #26 -> #72)
(var #27 Num *)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 -> #78)
(var #40 _)
(var #41 _)
(var #42 -> #76)
(var #43 -> #75)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 -> #78)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 -> #84)
(var #55 _)
(var #56 -> #83)
(var #57 _)
(var #58 -> #59)
(var #59 -> #60)
(var #60 Num *)
(var #61 _)
(var #62 -> #81)
(var #63 -> #82)
(var #64 _)
(var #65 -> #84)
(var #66 _)
(var #67 _)
(var #68 _)
(var #69 _)
(var #70 fn_pure)
(var #71 fn_pure)
(var #72 fn_pure)
(var #73 _)
(var #74 _)
(var #75 fn_pure)
(var #76 fn_pure)
(var #77 fn_pure)
(var #78 fn_pure)
(var #79 _)
(var #80 _)
(var #81 tuple)
(var #82 fn_pure)
(var #83 fn_pure)
(var #84 fn_pure)
~~~
# TYPES
~~~roc
y : _b
curriedAdd : _b
addThreeTwice : _arg -> _ret
applyTwice : _arg -> _arg2 -> _ret
f : _b
n : _b
makeAdder : _arg -> _arg2 -> _ret
x : _b
~~~
