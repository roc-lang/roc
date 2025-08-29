# META
~~~ini
description=Example of importing a nominal tag union from a module within a package, and renaming it using `as`
type=file
~~~
# SOURCE
~~~roc
module [red]

import design.Styles.Color exposing [Encoder as CE]

red : CE
red = ... # not implemented
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare KwImport LowerIdent Dot UpperIdent Dot UpperIdent KwExposing OpenSquare UpperIdent KwAs UpperIdent CloseSquare LowerIdent OpColon UpperIdent LowerIdent OpAssign TripleDot ~~~
# PARSE
~~~clojure
(block
  (import
    (binop_exposing
      (binop_pipe
        (binop_pipe
          (lc "design")
          (uc "Styles")
        )
        (uc "Color")
      )
      (list_literal
        (uc "Encoder")
      )
    )
  )
  (malformed malformed:expr_unexpected_token)
  (uc "CE")
  (malformed malformed:expr_unexpected_token)
  (binop_colon
    (lc "red")
    (uc "CE")
  )
  (binop_equals
    (lc "red")
    (ellipsis)
  )
)
~~~
# FORMATTED
~~~roc
module [red]

import design.Styles | Color exposing [Encoder]
as CE]

red : CE
red = ... # not implemented
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 3:46 to 3:49

**Parse Error**
at 3:51 to 5:1

**Unsupported Node**
at 3:1 to 3:45

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "red")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "red")
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
red : Error
~~~
