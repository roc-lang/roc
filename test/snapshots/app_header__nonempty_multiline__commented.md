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
  (packages))
~~~
# FORMATTED
~~~roc
app { }

# Comment after keyword
# Comment after packages open
# Comment after provides open
# Comment after exposed item
# Comment after platform
# Comment after last package
~~~
# EXPECTED
NIL
# PROBLEMS
**EXPECTED OPEN CURLY BRACE**
A parsing error occurred: **expected_package_platform_open_curly**
This is an unexpected parsing error. Please check your syntax.

**app_header__nonempty_multiline__commented.md:1:1:1:5:**
```roc
app # Comment after keyword
```
^^^^


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
~~~
