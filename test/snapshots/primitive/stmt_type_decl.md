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
  (Stmt.type_anno
    (name node:apply_uc)
    (type tuple_literal)
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
