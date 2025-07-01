# META
~~~ini
description=Match expression with tag patterns for different cases
type=expr
~~~
# SOURCE
~~~roc
match value {
    Answer => 1
    Zero => 2
    Greeting => 3
    Other => 4
}
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize match expression
Let us know if you want to help!

# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:12),OpenCurly(1:13-1:14),Newline(1:1-1:1),
UpperIdent(2:5-2:11),OpFatArrow(2:12-2:14),Int(2:15-2:16),Newline(1:1-1:1),
UpperIdent(3:5-3:9),OpFatArrow(3:10-3:12),Int(3:13-3:14),Newline(1:1-1:1),
UpperIdent(4:5-4:13),OpFatArrow(4:14-4:16),Int(4:17-4:18),Newline(1:1-1:1),
UpperIdent(5:5-5:10),OpFatArrow(5:11-5:13),Int(5:14-5:15),Newline(1:1-1:1),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.12 (qaul "") (raw "value"))
	(branches
		(branch @2.5-3.9
			(p-tag @2.5-2.11 (raw "Answer"))
			(e-int @2.15-2.16 (raw "1")))
		(branch @3.5-4.13
			(p-tag @3.5-3.9 (raw "Zero"))
			(e-int @3.13-3.14 (raw "2")))
		(branch @4.5-5.10
			(p-tag @4.5-4.13 (raw "Greeting"))
			(e-int @4.17-4.18 (raw "3")))
		(branch @5.5-6.2
			(p-tag @5.5-5.10 (raw "Other"))
			(e-int @5.14-5.15 (raw "4")))))
~~~
# FORMATTED
~~~roc
match value {
	Answer => 1
	Zero => 2
	Greeting => 3
	Other => 4
}
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Error"))
~~~
