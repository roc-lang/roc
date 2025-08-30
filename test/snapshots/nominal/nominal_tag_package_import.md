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
KwModule OpenSquare LowerIdent CloseSquare KwImport LowerIdent Dot UpperIdent KwAs UpperIdent LowerIdent OpColon UpperIdent Dot UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent Dot UpperIdent OpenRound Int Comma Int Comma Int CloseRound ~~~
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

import styles.Color as CC
blue : CC.Color
blue = CC.Color | RGB((0, 0, 255))
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 4:1 to 4:26

**Unsupported Node**
at 8:8 to 8:10

**Unsupported Node**
at 8:10 to 8:16

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "blue")
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "blue")
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
blue : _a
~~~
