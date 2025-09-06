# META
~~~ini
description=Multiple unqualified type declarations
type=file
~~~
# SOURCE
~~~roc
module []

FirstType : U64
SecondType : Str
ThirdType : List(U8)
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent UpperIdent OpColon UpperIdent OpenRound UpperIdent CloseRound ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

FirstType : U64
SecondType : Str
ThirdType : List U8
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.type_alias)
  (Stmt.type_alias)
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
