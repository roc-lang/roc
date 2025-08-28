# META
~~~ini
description=Multiline without comma formatting module
type=file
~~~
# SOURCE
~~~roc
module [
	a,
	b
]

a = 'a'
b = 'a'
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign SingleQuote LowerIdent OpAssign SingleQuote ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "a")
    (str_literal_small "a")
  )
  (binop_equals
    (lc "b")
    (str_literal_small "a")
  )
)
~~~
# FORMATTED
~~~roc
module [a, b]

a = 'a'
b = 'a'
b = 'a'
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_c")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
