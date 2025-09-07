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
(block
  (import
    (binop_dot
      (lc "pf")
      (uc "Stdout")
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (apply_anon
          (binop_dot
            (uc "Stdout")
            (not_lc "line")
          )
          (str_literal_big "Hello, world!")
        )
      )
      (args
        (underscore)
      )
    )
  )
)
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
Nothing is named **.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**hello_world.md:5:19:5:25:**
```roc
main! = |_| Stdout.line!("Hello, world!")
```
                  ^^^^^^


**SHADOWING**
This definition shadows an existing one.

**hello_world.md:5:1:5:6:**
```roc
main! = |_| Stdout.line!("Hello, world!")
```
^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



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
; Total type variables: 24
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 -> #23)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 -> #22)
(var #16 Str)
(var #17 _)
(var #18 -> #23)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 fn_pure)
(var #23 fn_pure)
~~~
# TYPES
~~~roc
main : _arg -> _ret
~~~
