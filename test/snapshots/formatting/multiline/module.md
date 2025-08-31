# META
~~~ini
description=Multiline formatting module
type=file
~~~
# SOURCE
~~~roc
module [
	a,
	b,
]

a = 'a'
b = 'a'
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent Comma CloseSquare BlankLine LowerIdent OpAssign SingleQuote LowerIdent OpAssign SingleQuote ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "a")

    (lc "b")
))
~~~
# FORMATTED
~~~roc
module [
	a,
	b,
]

a = 'a'
b = 'a'
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "a"))
    (Expr.str_literal_small)
  )
  (Stmt.assign
    (pattern (Patt.ident "b"))
    (Expr.str_literal_small)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
