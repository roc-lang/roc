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
(header-only)
~~~
# FORMATTED
~~~roc
package [] packages {d, {
	{
		d : {
			0
		} | .c
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
; No expression to type check
~~~
# TYPES
~~~roc
# No top-level expression found in file
~~~
