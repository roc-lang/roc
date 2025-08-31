# META
~~~ini
description=Hello world
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

import pf.Stdout

main! = |_| Stdout.line!("Hello, world!")
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar UpperIdent Dot LowerIdent OpBang OpenRound String CloseRound ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

import pf.Stdout

main! = |_| Stdout.line!("Hello, world!")
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **Stdout.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**hello_world.md:5:13:5:25:**
```roc
main! = |_| Stdout.line!("Hello, world!")
```
            ^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.assign
    (pattern (Patt.ident "main"))
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
