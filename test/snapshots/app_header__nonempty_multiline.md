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
  (packages))
~~~
# FORMATTED
~~~roc
app { }

# This comment is here
~~~
# EXPECTED
NIL
# PROBLEMS
**EXPECTED OPEN CURLY BRACE**
A parsing error occurred: **expected_package_platform_open_curly**
This is an unexpected parsing error. Please check your syntax.

**app_header__nonempty_multiline.md:1:1:1:5:**
```roc
app # This comment is here
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
