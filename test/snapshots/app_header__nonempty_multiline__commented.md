# META
~~~ini
description=App Header - nonempty multiline - heavily commented
type=header
~~~
# SOURCE
~~~roc
app # Comment after keyword
	{ # Comment after packages open
		pf: "../main.roc" platform [ # Comment after provides open
			main!, # Comment after exposed item
		], # Comment after platform
		other: "../../other/main.roc", # Comment after last package
	}
~~~
# TOKENS
~~~text
KwApp LineComment OpenCurly LineComment LowerIdent OpColon String KwPlatform OpenSquare LineComment LowerIdent OpBang Comma LineComment CloseSquare Comma LineComment LowerIdent OpColon String Comma LineComment CloseCurly ~~~
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
      (lc "other")
      (str_literal_big "../../other/main.roc")
    )
))
~~~
# FORMATTED
~~~roc
app
{
	# Comment after keyword
	# Comment after packages open
	pf: "../main.roc" platform [ # Comment after provides open
		main!
	],
	# Comment after exposed item
	# Comment after platform
	other: "../../other/main.roc",
}# Comment after last package
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **main!** in this scope.
Is there an **import** or **exposing** missing up-top?

**app_header__nonempty_multiline__commented.md:4:4:4:9:**
```roc
			main!, # Comment after exposed item
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
