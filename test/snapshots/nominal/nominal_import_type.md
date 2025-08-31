# META
~~~ini
description=Example of importing a nominal tag union from another module
type=file
~~~
# SOURCE
~~~roc
module [red]

import Color

red : Color.RGB
red = Color.RGB.Red
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine KwImport UpperIdent BlankLine LowerIdent OpColon UpperIdent Dot UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "red")
))
~~~
# FORMATTED
~~~roc
module [red]

import Color

red : Color.RGB
red = (Color.RGB | Red)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.type_anno
    (name "red")
    (type binop_pipe)
  )
  (Stmt.assign
    (pattern (Patt.ident "red"))
    (Expr.lambda (canonicalized))
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
