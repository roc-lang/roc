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
(block
  (import
    (lc "styles")
    (uc "Color")
    (uc "CC")
  )
  (binop_colon
    (lc "blue")
    (binop_pipe
      (uc "CC")
      (uc "Color")
    )
  )
  (binop_equals
    (lc "blue")
    (apply_anon
      (binop_pipe
        (binop_pipe
          (uc "CC")
          (uc "Color")
        )
        (uc "RGB")
      )
      (tuple_literal
        (num_literal_i32 0)
        (num_literal_i32 0)
        (num_literal_i32 255)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
MODULE NOT FOUND - nominal_tag_package_import.md:4:1:4:26
# PROBLEMS
**Unsupported Node**
at 4:1 to 4:26

**Unsupported Node**
at 7:8 to 7:10

**Unsupported Node**
at 8:8 to 8:10

**Unsupported Node**
at 8:10 to 8:15

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "blue")
    (Expr.lambda)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
