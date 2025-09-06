# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
package[]{d:{{d:{0}?}}}
~~~
# TOKENS
~~~text
KwPackage OpenSquare CloseSquare OpenCurly LowerIdent OpColon OpenCurly OpenCurly LowerIdent OpColon OpenCurly Int CloseCurly OpQuestion CloseCurly CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(package-header
  (packages
    (lc "d")

    (block
      (block
        (binop_colon
          (lc "d")
          (block
            (num_literal_i32 0)
          )
        )
        (malformed)
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
		}
		?
	}
}}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_071.md:1:20:1:21:**
```roc
package[]{d:{{d:{0}?}}}
```
                   ^


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
