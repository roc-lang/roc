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
MODULE NOT FOUND - nominal_import_type.md:3:1:3:13
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_import_type.md:6:7:6:20:**
```roc
red = Color.RGB.Red
```
      ^^^^^^^^^^^^^


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
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
