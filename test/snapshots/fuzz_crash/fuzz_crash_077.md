# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
package[]{d:{{d:||{0}->R}}}
~~~
# TOKENS
~~~text
KwPackage OpenSquare CloseSquare OpenCurly LowerIdent OpColon OpenCurly OpenCurly LowerIdent OpColon OpOr OpenCurly Int CloseCurly OpArrow UpperIdent CloseCurly CloseCurly CloseCurly ~~~
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
        (malformed malformed:expr_unexpected_token)
        (uc "R")
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
		->
		R
	}
}}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **||** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_077.md:1:17:1:19:**
```roc
package[]{d:{{d:||{0}->R}}}
```
                ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **->** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_077.md:1:22:1:24:**
```roc
package[]{d:{{d:||{0}->R}}}
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
