# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
package[]{d:{0}}
~~~
# TOKENS
~~~text
KwPackage OpenSquare CloseSquare OpenCurly LowerIdent OpColon OpenCurly Int CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(package-header
  (packages
    (lc "d")

    (block
      (num_literal_i32 0)
    )
))
~~~
# FORMATTED
~~~roc
package [] packages {d, {
	0
}}
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
; Total type variables: 0
~~~
# TYPES
~~~roc
~~~
