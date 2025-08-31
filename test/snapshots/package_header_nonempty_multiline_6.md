# META
~~~ini
description=package_header_nonempty_multiline (6)
type=file
~~~
# SOURCE
~~~roc
package # Comment after keyword
	[ # Comment after exposes open
		something, # Comment after exposed item
		SomeType, # Comment after last exposed item
	]
	{ # Comment after packages open
		somePkg: "../main.roc", # Comment after package
		other: "../../other/main.roc", # Comment after last package
	}
~~~
# TOKENS
~~~text
KwPackage LineComment OpenSquare LineComment LowerIdent Comma LineComment UpperIdent Comma LineComment CloseSquare OpenCurly LineComment LowerIdent OpColon String Comma LineComment LowerIdent OpColon String Comma LineComment CloseCurly ~~~
# PARSE
~~~clojure
(package-header
  (exposes
    (lc "something")

    (uc "SomeType")
)
  (packages
    (lc "somePkg")

    (binop_colon
      (tuple_literal
        (str_literal_big "../main.roc")
        (lc "other")
      )
      (str_literal_big "../../other/main.roc")
    )
))
~~~
# FORMATTED
~~~roc
package [ # Comment after keyword
	# Comment after exposes open
	something,
	# Comment after exposed item
	SomeType,
] packages { # Comment after last exposed item
# Comment after packages open
somePkg, (
	"../main.roc", # Comment after package
	other,
) : "../../other/main.roc"}

# Comment after last package
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
; No expression to type check
~~~
# TYPES
~~~roc
# No top-level expression found in file
~~~
