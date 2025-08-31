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
          (malformed)
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
**UNEXPECTED TOKEN IN EXPRESSION**
The token **||** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_075.md:1:17:1:19:**
```roc
package[]{d:{{d:||{0}}}}
```
                ^^


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
