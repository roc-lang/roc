# META
~~~ini
description=Test crash statement inside a block
type=expr
~~~
# SOURCE
~~~roc
{
    crash "error message"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
KwCrash,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-crash
			(e-string
				(e-string-part (raw "error message"))))))
~~~
# FORMATTED
~~~roc
{
	crash "error message"
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(e-crash (msg "error message")))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
