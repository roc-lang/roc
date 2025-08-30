# META
~~~ini
description=Debug expression in a block context both statement and expression versions
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo = |num| {
    # statement - prints out the value of num convertert to a string
    dbg num.to_str()

    # expression - prints out the value of num and then returns it
    dbg(num)
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly KwDbg LowerIdent Dot LowerIdent OpenRound CloseRound KwDbg OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "foo")
))
~~~
# FORMATTED
~~~roc
module [foo]

foo = |num| {
	dbg 
	num.to_str()
	dbg(num)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 5:5 to 5:9

**Parse Error**
at 8:5 to 8:8

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "foo")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
foo : _a
~~~
