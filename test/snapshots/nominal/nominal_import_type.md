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
(block
  (import
    (uc "Color")
  )
  (binop_colon
    (lc "red")
    (binop_dot
      (uc "Color")
      (uc "RGB")
    )
  )
  (binop_equals
    (lc "red")
    (binop_dot
      (binop_dot
        (uc "Color")
        (uc "RGB")
      )
      (uc "Red")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [red]

import Color
red : Color.RGB
red = (Color.RGB.Red)
~~~
# EXPECTED
MODULE NOT FOUND - nominal_import_type.md:3:1:3:13
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**nominal_import_type.md:6:1:6:4:**
```roc
red = Color.RGB.Red
```
^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "red"))
    (type type_7)
  )
  (Stmt.assign
    (pattern (Patt.ident "red"))
    (Expr.record_access)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 17
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 -> #14)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
~~~
# TYPES
~~~roc
red : _a
~~~
