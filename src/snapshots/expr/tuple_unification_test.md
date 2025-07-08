# META
~~~ini
description=Tuple unification with different types
type=expr
~~~
# SOURCE
~~~roc
[(1, "a"), (2.5, "b")]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),NoSpaceOpenRound(1:2-1:3),Int(1:3-1:4),Comma(1:4-1:5),StringStart(1:6-1:7),StringPart(1:7-1:8),StringEnd(1:8-1:9),CloseRound(1:9-1:10),Comma(1:10-1:11),OpenRound(1:12-1:13),Float(1:13-1:16),Comma(1:16-1:17),StringStart(1:18-1:19),StringPart(1:19-1:20),StringEnd(1:20-1:21),CloseRound(1:21-1:22),CloseSquare(1:22-1:23),EndOfFile(1:23-1:23),
~~~
# PARSE
~~~clojure
(e-list @1.1-1.23
	(e-tuple @1.2-1.10
		(e-int @1.3-1.4 (raw "1"))
		(e-string @1.6-1.9
			(e-string-part @1.7-1.8 (raw "a"))))
	(e-tuple @1.12-1.22
		(e-frac @1.13-1.16 (raw "2.5"))
		(e-string @1.18-1.21
			(e-string-part @1.19-1.20 (raw "b")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-1.23
	(elems
		(e-tuple @1.2-1.10
			(elems
				(e-int @1.3-1.4 (value "1"))
				(e-str @1.6-1.9
					(e-literal @1.7-1.8 (string "a")))))
		(e-tuple @1.12-1.22
			(elems
				(e-dec-small @1.13-1.16 (numerator "25") (denominator-power-of-ten "1") (value "2.5"))
				(e-str @1.18-1.21
					(e-literal @1.19-1.20 (string "b")))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.23 (type "List((Frac(*), Str))"))
~~~
