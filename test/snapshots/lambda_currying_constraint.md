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
KwModule OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare LowerIdent OpColon LowerIdent OpArrow OpenRound LowerIdent OpArrow LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpColon OpenRound LowerIdent OpArrow LowerIdent CloseRound Comma LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent OpenRound LowerIdent OpenRound LowerIdent CloseRound CloseRound LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpenRound OpBar LowerIdent OpBar LowerIdent OpPlus Int Comma LowerIdent CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "makeAdder")
    (binop_thin_arrow
      (lc "a")
      (binop_thin_arrow
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
    (binop_thin_arrow
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
    (binop_thin_arrow
      (binop_thin_arrow
        (lc "a")
        (lc "a")
      )
      (binop_thin_arrow
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
        (tuple_literal
          (lc "f")
          (lc "x")
        )
      )
    )
  )
  (binop_colon
    (lc "addThreeTwice")
    (binop_thin_arrow
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
NO CHANGE
~~~
# EXPECTED
TYPE MISMATCH - lambda_currying_constraint.md:5:21:5:22
# PROBLEMS
**Unsupported Node**
at 4:13 to 4:25

**Unsupported Node**
at 5:13 to 5:17

**Unsupported Node**
at 8:14 to 8:24

**Unsupported Node**
at 12:15 to 12:30

**Unsupported Node**
at 13:14 to 13:21

**Unsupported Node**
at 16:17 to 16:27

**Unsupported Node**
at 17:17 to 17:21

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "makeAdder")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "curriedAdd")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "applyTwice")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "addThreeTwice")
    (Expr.malformed)
  )
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
