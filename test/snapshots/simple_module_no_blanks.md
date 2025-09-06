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
(block
  (import
    (binop_dot
      (lc "pf")
      (uc "Stdout")
    )
  )
  (binop_equals
    (not_lc "hello")
    (apply_anon
      (binop_dot
        (uc "Stdout")
        (not_lc "line")
      )
      (str_literal_big "Hello")
    )
  )
  (binop_equals
    (lc "world")
    (str_literal_big "World")
  )
)
~~~
# FORMATTED
~~~roc
module [hello!, world]

import pf.Stdout
hello! = Stdout.line!("Hello")
world = "World"
~~~
# EXPECTED
MODULE NOT FOUND - simple_module_no_blanks.md:2:1:2:17
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**simple_module_no_blanks.md:3:16:3:22:**
```roc
hello! = Stdout.line!("Hello")
```
               ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.assign
    (pattern (Patt.ident "hello"))
    (Expr.fn_call)
  )
  (Stmt.assign
    (pattern (Patt.ident "world"))
    (Expr.str_literal_big)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 19
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 -> #12)
(var #8 _)
(var #9 _)
(var #10 -> #18)
(var #11 Str)
(var #12 _)
(var #13 _)
(var #14 -> #15)
(var #15 Str)
(var #16 _)
(var #17 _)
(var #18 fn_pure)
~~~
# TYPES
~~~roc
hello : _a
world : Str
~~~
