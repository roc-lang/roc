# META
~~~ini
description=Match expression with multiple patterns in one branch
type=expr
~~~
# SOURCE
~~~roc
match color {
    Blue | Green | Red => 1
    Black => 2
    White => 3
}
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize match expression

# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:12),OpenCurly(1:13-1:14),Newline(1:1-1:1),
UpperIdent(2:5-2:9),OpBar(2:10-2:11),UpperIdent(2:12-2:17),OpBar(2:18-2:19),UpperIdent(2:20-2:23),OpFatArrow(2:24-2:26),Int(2:27-2:28),Newline(1:1-1:1),
UpperIdent(3:5-3:10),OpFatArrow(3:11-3:13),Int(3:14-3:15),Newline(1:1-1:1),
UpperIdent(4:5-4:10),OpFatArrow(4:11-4:13),Int(4:14-4:15),Newline(1:1-1:1),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.12 (qaul "") (raw "color"))
	(branches
		(branch @2.5-3.10
			(p-alternatives
				(p-tag @2.5-2.9 (raw "Blue"))
				(p-tag @2.12-2.17 (raw "Green"))
				(p-tag @2.20-2.23 (raw "Red")))
			(e-int @2.27-2.28 (raw "1")))
		(branch @3.5-4.10
			(p-tag @3.5-3.10 (raw "Black"))
			(e-int @3.14-3.15 (raw "2")))
		(branch @4.5-5.2
			(p-tag @4.5-4.10 (raw "White"))
			(e-int @4.14-4.15 (raw "3")))))
~~~
# FORMATTED
~~~roc
match color {
	Blue | Green | Red => 1
	Black => 2
	White => 3
}
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
