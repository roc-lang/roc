# META
~~~ini
description=Example of a nominal tag union import from a package
type=file
~~~
# SOURCE
~~~roc
module [blue]

# import the Color module from styles package as CC
import styles.Color as CC

# instantiating an RGB nominal tab union from the styles.Color module
blue : CC.Color
blue = CC.Color.RGB(0,0,255)
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LineComment KwImport LowerIdent Dot UpperIdent KwAs UpperIdent BlankLine LineComment LowerIdent OpColon UpperIdent Dot UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent Dot UpperIdent OpenRound Int Comma Int Comma Int CloseRound ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "blue")
))
~~~
# FORMATTED
~~~roc
module [blue]

# import the Color module from styles package as CC
import styles.Color as CC

# instantiating an RGB nominal tab union from the styles.Color module
blue : CC.Color
blue = CC.Color | RGB((0, 0, 255))
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
    (name "blue")
    (type binop_pipe)
  )
  (Stmt.assign
    (pattern (Patt.ident "blue"))
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
