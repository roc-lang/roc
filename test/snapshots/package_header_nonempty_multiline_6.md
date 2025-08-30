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
KwPackage OpenSquare LowerIdent Comma UpperIdent Comma CloseSquare OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon String Comma CloseCurly ~~~
# PARSE
~~~clojure
(package-header
  (exposes
    (lc "something")

    (uc "SomeType")
)
  (packages
    (lc "somePkg")

    (tuple_literal
      (binop_colon
        (tuple_literal
          (str_literal_big "../main.roc")
          (lc "other")
        )
        (str_literal_big "../../other/main.roc")
      )
      (malformed malformed:expr_unexpected_token)
    )
))
~~~
# FORMATTED
~~~roc
package [
	something,
	SomeType,
] packages {somePkg, (
	(
		"../main.roc",
		other,
	) : "../../other/main.roc",
)}

~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 9:2 to 9:3

**Expected Close Curly Brace**
at 1:1 to 9:3

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
