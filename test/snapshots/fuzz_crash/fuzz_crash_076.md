# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
package[]{d:if 0 0 else{{}}}
~~~
# TOKENS
~~~text
KwPackage OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwIf Int Int KwElse OpenCurly OpenCurly CloseCurly CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(package-header
  (packages
    (lc "d")

    (if_else
      (condition         (num_literal_i32 0)
)
      (then         (num_literal_i32 0)
)
      (else         (block
          (record_literal)
        )
))
))
~~~
# FORMATTED
~~~roc
package [] packages {d, if 0
	0
else {
	{}
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
