# META
~~~ini
description=Complex expressions with captures - lambda with conditionals and captures
type=expr
~~~
# SOURCE
~~~roc
(|outer| |inner| if outer > 0 (outer + inner) else inner)(1)(-2)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar OpBar LowerIdent OpBar KwIf LowerIdent OpGreaterThan Int OpenRound LowerIdent OpPlus LowerIdent CloseRound KwElse LowerIdent CloseRound OpenRound Int CloseRound OpenRound OpUnaryMinus Int CloseRound ~~~
# PARSE
~~~clojure
(lambda
  (body
    (lambda
      (body
        (if_without_else <6 branches>)
      )
      (args
        (lc "inner")
      )
    )
  )
  (args
    (lc "outer")
  )
)
~~~
# FORMATTED
~~~roc
|outer| |inner| if outer > 0(outer + inner) 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:18 to 1:47

**Parse Error**
at 1:47 to 1:47

**Parse Error**
at 1:52 to 1:52

# CANONICALIZE
~~~clojure
(Expr.lambda)
~~~
# SOLVED
~~~clojure
(expr :tag lambda :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
