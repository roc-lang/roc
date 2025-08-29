# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [Foo]

Foo(a,b) : (a,b,Str,U64)
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent CloseSquare UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenRound LowerIdent Comma LowerIdent Comma UpperIdent Comma UpperIdent CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (apply_uc
      (uc "Foo")
      (tuple_literal
        (lc "a")
        (lc "b")
      )
    )
    (tuple_literal
      (lc "a")
      (lc "b")
      (uc "Str")
      (uc "U64")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [Foo]

Foo((a, b)) : (a, b, Str, U64)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.tuple_literal
      (Expr.lookup "a")
      (Expr.lookup "b")
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag record_literal :type "{}")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
