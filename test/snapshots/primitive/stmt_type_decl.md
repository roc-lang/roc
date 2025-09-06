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
KwModule OpenSquare UpperIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenRound LowerIdent Comma LowerIdent Comma UpperIdent Comma UpperIdent CloseRound ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "Foo")
))
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
(Expr.block
  (Stmt.type_alias)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 14
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
~~~
# TYPES
~~~roc
~~~
