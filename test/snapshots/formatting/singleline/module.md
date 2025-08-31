# META
~~~ini
description=Singleline formatting module
type=file
~~~
# SOURCE
~~~roc
module [a, b]

a = 'a'
b = 'a'
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare BlankLine LowerIdent OpAssign SingleQuote LowerIdent OpAssign SingleQuote ~~~
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
module [a, b]


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
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
