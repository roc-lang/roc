# META
~~~ini
description=A simple module with no blanks
type=file
~~~
# SOURCE
~~~roc
module [hello!, world]
import pf.Stdout
hello! = Stdout.line!("Hello")
world = "World"
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent OpBang Comma LowerIdent CloseSquare KwImport LowerIdent Dot UpperIdent LowerIdent OpBang OpAssign UpperIdent Dot LowerIdent OpBang OpenRound String CloseRound LowerIdent OpAssign String ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (not_lc "hello")

    (lc "world")
))
~~~
# FORMATTED
~~~roc
module [hello!, world]

import pf.Stdout
hello! = Stdout.line!("Hello")
world = "World"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**simple_module_no_blanks.md:2:1:2:17:**
```roc
import pf.Stdout
```
^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.not_lookup)
    (Expr.apply_ident)
  )
  (Expr.binop_equals
    (Expr.lookup "world")
    (Expr.str_literal_big)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
world : Str
~~~
