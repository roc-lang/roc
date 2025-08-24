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
    (lc "design")
    (uc "Styles")
    (uc "Color")
    (uc "Encoder")
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
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - nominal_import_long_package.md:3:21:3:27
PARSE ERROR - nominal_import_long_package.md:3:28:3:36
PARSE ERROR - nominal_import_long_package.md:3:37:3:38
PARSE ERROR - nominal_import_long_package.md:3:46:3:48
PARSE ERROR - nominal_import_long_package.md:3:51:3:52
MODULE NOT FOUND - nominal_import_long_package.md:3:1:3:21
UNDECLARED TYPE - nominal_import_long_package.md:5:7:5:9
# PROBLEMS
**Parse Error**
at 3:46 to 3:46

**Parse Error**
at 3:51 to 3:51

**Unsupported Node**
at 3:1 to 3:45

**Unsupported Node**
at 3:46 to 3:46

**Unsupported Node**
at 3:51 to 3:51

**Unsupported Node**
at 6:7 to 6:10

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
