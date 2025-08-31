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
**UNDEFINED VARIABLE**
Nothing is named **Stdout.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**simple_module_no_blanks.md:3:10:3:22:**
```roc
hello! = Stdout.line!("Hello")
```
         ^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
