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
addThreeTwice = |n| applyTwice(|x| (x + 3, n))# Function that returns a function with polymorphic type
# Should constrain the literal 5 to I64
# Higher-order function that applies a function twice
# Should constrain the literal 3 to I64
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
