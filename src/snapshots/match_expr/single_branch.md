# META
~~~ini
description=Match expression with single branch (simple variable pattern)
type=expr
~~~
# SOURCE
~~~roc
match value {
    x => x + 1
}
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `value` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:12),OpenCurly(1:13-1:14),Newline(1:1-1:1),
LowerIdent(2:5-2:6),OpFatArrow(2:7-2:9),LowerIdent(2:10-2:11),OpPlus(2:12-2:13),Int(2:14-2:15),Newline(1:1-1:1),
CloseCurly(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.12 (raw "value"))
	(branches
		(branch @2.5-3.2
			(p-ident @2.5-2.6 (raw "x"))
			(e-binop @2.10-3.2 (op "+")
				(e-ident @2.10-2.11 (raw "x"))
				(e-int @2.14-2.15 (raw "1"))))))
~~~
# FORMATTED
~~~roc
match value {
	x => x + 1
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-3.2
	(match @1.1-3.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(p-assign @2.5-2.6 (ident "x") (degenerate false)))
				(value
					(e-binop @2.10-3.2 (op "add")
						(e-lookup-local @2.10-2.11
							(pattern @2.5-2.6))
						(e-int @2.14-2.15 (value "1"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-3.2 (type "*"))
~~~
