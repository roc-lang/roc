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
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
