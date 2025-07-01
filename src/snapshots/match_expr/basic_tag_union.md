# META
~~~ini
description=Basic tag union match with simple patterns
type=expr
~~~
# SOURCE
~~~roc
match color {
    Red => 1
    Blue => 2
    Green => 3
}
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `color` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:12),OpenCurly(1:13-1:14),Newline(1:1-1:1),
UpperIdent(2:5-2:8),OpFatArrow(2:9-2:11),Int(2:12-2:13),Newline(1:1-1:1),
UpperIdent(3:5-3:9),OpFatArrow(3:10-3:12),Int(3:13-3:14),Newline(1:1-1:1),
UpperIdent(4:5-4:10),OpFatArrow(4:11-4:13),Int(4:14-4:15),Newline(1:1-1:1),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.12 (qaul "") (raw "color"))
	(branches
		(branch @2.5-3.9
			(p-tag @2.5-2.8 (raw "Red"))
			(e-int @2.12-2.13 (raw "1")))
		(branch @3.5-4.10
			(p-tag @3.5-3.9 (raw "Blue"))
			(e-int @3.13-3.14 (raw "2")))
		(branch @4.5-5.2
			(p-tag @4.5-4.10 (raw "Green"))
			(e-int @4.14-4.15 (raw "3")))))
~~~
# FORMATTED
~~~roc
match color {
	Red => 1
	Blue => 2
	Green => 3
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-5.2
	(match @1.1-5.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(p-applied-tag @2.5-2.8 (degenerate false)))
				(value
					(e-int @2.12-2.13 (value "1"))))
			(branch
				(patterns
					(p-applied-tag @3.5-3.9 (degenerate false)))
				(value
					(e-int @3.13-3.14 (value "2"))))
			(branch
				(patterns
					(p-applied-tag @4.5-4.10 (degenerate false)))
				(value
					(e-int @4.14-4.15 (value "3")))))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "*"))
~~~
