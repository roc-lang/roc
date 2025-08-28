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
(header-only)
~~~
# FORMATTED
~~~roc
package [] packages {d, {
	{
		d : 
		{
			0
		}
		
		R
	}
}}

~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:17 to 1:17

**Parse Error**
at 1:22 to 1:22

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
