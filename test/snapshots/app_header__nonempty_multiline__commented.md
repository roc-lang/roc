# META
~~~ini
description=App Header - nonempty multiline - heavily commented
type=header
~~~
# SOURCE
~~~roc
app # Comment after keyword
	[ # Comment after provides open
		main!, # Comment after exposed item
	]
	{ # Comment after packages open
		pf: platform "../main.roc", # Comment after platform
		other: "../../other/main.roc", # Comment after last package
	}
~~~
# TOKENS
~~~text
KwApp LineComment OpenSquare LineComment LowerIdent OpBang Comma LineComment CloseSquare OpenCurly LineComment LowerIdent OpColon KwPlatform String Comma LineComment LowerIdent OpColon String Comma LineComment CloseCurly ~~~
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
        (str_literal_big "../main.roc")
        (block)
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
app [ # Comment after keyword
	# Comment after provides open
	main!,
]
{
	# Comment after exposed item
	# Comment after packages open
	pf: "../main.roc" platform [],
	# Comment after platform
	other: "../../other/main.roc",
}# Comment after last package
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(empty)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
