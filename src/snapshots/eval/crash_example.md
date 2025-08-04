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
OpenCurly(1:1-1:2),
KwCrash(2:5-2:10),StringStart(2:11-2:12),StringPart(2:12-2:37),StringEnd(2:37-2:38),
OpenCurly(3:5-3:6),CloseCurly(3:6-3:7),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-4.2
	(statements
		(s-crash @2.5-2.38
			(e-string @2.11-2.38
				(e-string-part @2.12-2.37 (raw "This is a crash statement"))))
		(e-record @3.5-3.7)))
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
(e-block @1.1-4.2
	(s-crash @2.5-2.38 (msg "This is a crash statement"))
	(e-empty_record @3.5-3.7))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "{}"))
~~~
