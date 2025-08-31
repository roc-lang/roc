# META
~~~ini
description=Type annotation connection to definitions
type=file
~~~
# SOURCE
~~~roc
module [add_one, my_number]

add_one : U64 -> U64
add_one = |x| x + 1

my_number : U64
my_number = add_one(42)
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Int BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign LowerIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "add_one")

    (lc "my_number")
))
~~~
# FORMATTED
~~~roc
module [add_one, my_number]

add_one : U64 -> U64
add_one = |x| x + 1

my_number : U64
my_number = add_one(42)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "add_one")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "add_one"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "my_number")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "my_number"))
    (Expr.apply_ident)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
