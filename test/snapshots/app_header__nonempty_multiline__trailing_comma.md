# META
~~~ini
description=App Header - nonempty multiline w/ trailing comma
type=header
~~~
# SOURCE
~~~roc
app
	{ pf: "../main.roc" platform [main!,], somePkg: "../main.roc", }
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang Comma CloseSquare Comma LowerIdent OpColon String Comma CloseCurly ~~~
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
app
{
	pf: "../main.roc" platform [main!],
	somePkg: "../main.roc",
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **main!** in this scope.
Is there an **import** or **exposing** missing up-top?

**app_header__nonempty_multiline__trailing_comma.md:2:32:2:37:**
```roc
	{ pf: "../main.roc" platform [main!,], somePkg: "../main.roc", }
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
