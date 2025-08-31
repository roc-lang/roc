# META
~~~ini
description=Type redeclaration in same scope should produce error
type=file
~~~
# SOURCE
~~~roc
module [Maybe]

Maybe(a) : [Some(a), None]
Maybe(a) : [Ok(a), Err]
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent CloseSquare UpperIdent OpenRound LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent CloseSquare ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "Maybe")
))
~~~
# FORMATTED
~~~roc
module [Maybe]

Maybe(a) : [Some(a), None]
Maybe(a) : [Ok(a), Err]
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
    (type list_literal)
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type list_literal)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
