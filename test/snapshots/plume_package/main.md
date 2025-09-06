# META
~~~ini
description=main module from package
type=package
~~~
# SOURCE
~~~roc
package [
    Color,
] {}
~~~
# TOKENS
~~~text
KwPackage OpenSquare UpperIdent Comma CloseSquare OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(package-header
  (exposes
    (uc "Color")
))
~~~
# FORMATTED
~~~roc
package [
	Color,
]
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - main.md:2:5:2:10
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(empty)
~~~
# SOLVED
~~~clojure
; Total type variables: 0
~~~
# TYPES
~~~roc
~~~
