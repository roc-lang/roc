# META
~~~ini
description=App Header = nonempty multiline
type=header
~~~
# SOURCE
~~~roc
app # This comment is here
	{ pf: "../main.roc" platform [main!], somePkg: "../main.roc" }
~~~
# TOKENS
~~~text
KwApp LineComment OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare Comma LowerIdent OpColon String CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../main.roc")
        (block
          (not_lc "main")
        )
      )
    )

    (binop_colon
      (lc "somePkg")
      (str_literal_big "../main.roc")
    )
))
~~~
# FORMATTED
~~~roc
app { # This comment is here
pf: "../main.roc" platform [main!], somePkg: "../main.roc" }
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **main!** in this scope.
Is there an **import** or **exposing** missing up-top?

**app_header__nonempty_multiline.md:2:32:2:37:**
```roc
	{ pf: "../main.roc" platform [main!], somePkg: "../main.roc" }
```
	                              ^^^^^


# CANONICALIZE
~~~clojure
(empty)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
main : _a
~~~
