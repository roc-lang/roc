# META
~~~ini
description=Example if-then-else statement
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo = if 1 A

    else {
	"hello"
    }
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpAssign KwIf Int UpperIdent KwElse OpenCurly String CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "foo")
    (if_else <0 branches>)
  )
)
~~~
# FORMATTED
~~~roc
module [foo]

foo = if 1
	A
else {
	"hello"
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 3:7 to 3:12

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "foo")
    (Expr.if_else)
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
