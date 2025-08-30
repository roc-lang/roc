# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
package[]{d:{{d:||{0}}}}
~~~
# TOKENS
~~~text
KwPackage OpenSquare CloseSquare OpenCurly LowerIdent OpColon OpenCurly OpenCurly LowerIdent OpColon OpOr OpenCurly Int CloseCurly CloseCurly CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(package-header
  (packages
    (lc "d")

    (block
      (block
        (binop_colon
          (lc "d")
          (malformed malformed:expr_unexpected_token)
        )
        (block
          (num_literal_i32 0)
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
package [] packages {d, {
	{
		d : ||
		{
			0
		}
	}
}}

~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:17 to 1:19

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
