# META
~~~ini
description=Expression with a crash statement
type=expr
~~~
# SOURCE
~~~roc
{
    crash "This is a crash statement"
    {}
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
OpenCurly,CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-crash
			(e-string
				(e-string-part (raw "This is a crash statement"))))
		(e-record)))
~~~
# FORMATTED
~~~roc
{
	crash "This is a crash statement"
	{}
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-crash (msg "This is a crash statement"))
	(e-empty_record))
~~~
# TYPES
~~~clojure
(expr (type "{}"))
~~~
