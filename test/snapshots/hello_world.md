# META
~~~ini
description=Hello world
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

main! = |_| Stdout.line!("Hello, world!")
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar UpperIdent Dot LowerIdent OpBang OpenRound String CloseRound ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block)
      )
    )
))
~~~
# FORMATTED
~~~roc
app [main!] { pf: "../basic-cli/platform.roc" platform [] }

import pf.Stdout

main! = |_| Stdout.line!("Hello, world!")
~~~
# EXPECTED
MODULE NOT FOUND - hello_world.md:3:1:3:17
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **Stdout.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**hello_world.md:5:13:5:25:**
```roc
main! = |_| Stdout.line!("Hello, world!")
```
            ^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**hello_world.md:5:19:5:25:**
```roc
main! = |_| Stdout.line!("Hello, world!")
```
                  ^^^^^^


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
~~~
# TYPES
~~~roc
~~~
