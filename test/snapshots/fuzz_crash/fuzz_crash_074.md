# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
package[]{d:{{d:{0}.c}}}
~~~
# TOKENS
~~~text
KwPackage OpenSquare CloseSquare OpenCurly LowerIdent OpColon OpenCurly OpenCurly LowerIdent OpColon OpenCurly Int CloseCurly Dot LowerIdent CloseCurly CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(package-header
  (packages
    (lc "d")

    (block
      (block
        (binop_colon
          (lc "d")
          (binop_dot
            (block
              (num_literal_i32 0)
            )
            (dot_lc "c")
          )
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
package [] packages {d, {
	{
		d : {
			0
		}..c
	}
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
