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
module [makeAdder, curriedAdd, applyTwice]

makeAdder : a -> a -> a
makeAdder = |x| |y| x + y
curriedAdd : I64 -> I64
curriedAdd = makeAdder(5)
applyTwice :
	(a -> a) -> a -> a
applyTwice = |f, x| f(f(x))
addThreeTwice : I64 -> I64
addThreeTwice = |n| applyTwice(|x| (x + 3, n))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "makeAdder")
    (Expr.binop_thin_arrow
      (Expr.lookup "a")
      (Expr.binop_thin_arrow
        (Expr.lookup "a")
        (Expr.lookup "a")
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "makeAdder")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "curriedAdd")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "curriedAdd")
    (Expr.apply_ident)
  )
  (Expr.binop_colon
    (Expr.lookup "applyTwice")
    (Expr.binop_thin_arrow
      (Expr.binop_thin_arrow
        (Expr.lookup "a")
        (Expr.lookup "a")
      )
      (Expr.binop_thin_arrow
        (Expr.lookup "a")
        (Expr.lookup "a")
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "applyTwice")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "addThreeTwice")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "addThreeTwice")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
makeAdder : _b
curriedAdd : _b
applyTwice : _b
addThreeTwice : _b
~~~
