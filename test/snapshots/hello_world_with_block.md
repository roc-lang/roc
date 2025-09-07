# META
~~~ini
description=Hello world with a block
type=file
~~~
# SOURCE
~~~roc
# Hello world!

# Multiline comments?
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

main! = |_| {
	world = "World"
	# Hello
	Stdout.line!("Hello, world!")
}
~~~
# TOKENS
~~~text
LineComment BlankLine LineComment KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine KwImport LowerIdent Dot UpperIdent BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign String LineComment UpperIdent Dot LowerIdent OpBang OpenRound String CloseRound CloseCurly ~~~
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
        (block
          (binop_equals
            (lc "world")
            (str_literal_big "World")
          )
          (apply_anon
            (binop_dot
              (uc "Stdout")
              (not_lc "line")
            )
            (str_literal_big "Hello, world!")
          )
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
# Hello world!
# Multiline comments?
app [main!] { pf: "../basic-cli/platform.roc" platform [] }

import pf.Stdout
main! = |_| {
	world = "World"
	# Hello
	Stdout.line!("Hello, world!")
}
~~~
# EXPECTED
MODULE NOT FOUND - hello_world_with_block.md:6:1:6:17
UNUSED VARIABLE - hello_world_with_block.md:9:2:9:7
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **.line!** in this scope.
Is there an **import** or **exposing** missing up-top?

**hello_world_with_block.md:11:8:11:14:**
```roc
	Stdout.line!("Hello, world!")
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
; Total type variables: 28
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
(var #11 -> #27)
(var #12 _)
(var #13 -> #14)
(var #14 Str)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 -> #26)
(var #19 Str)
(var #20 _)
(var #21 _)
(var #22 -> #27)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 fn_pure)
(var #27 fn_pure)
~~~
# TYPES
~~~roc
main : _arg -> _ret
~~~
