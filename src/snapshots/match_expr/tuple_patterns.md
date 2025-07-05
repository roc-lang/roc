# META
~~~ini
description=Match expression with tuple destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match coord {
    (Zero, Zero) => "origin"
    (x, Zero) => x
    (Zero, y) => y
    (x, y) => x
}
~~~
# EXPECTED
UNDEFINED VARIABLE - tuple_patterns.md:5:9:5:10
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `coord` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNUSED VARIABLE**
Variable ``y`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_y` to suppress this warning.
The unused variable is declared here:
**tuple_patterns.md:5:9:5:10:**
```roc
    (x, y) => x
```
        ^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:12),OpenCurly(1:13-1:14),Newline(1:1-1:1),
OpenRound(2:5-2:6),UpperIdent(2:6-2:10),Comma(2:10-2:11),UpperIdent(2:12-2:16),CloseRound(2:16-2:17),OpFatArrow(2:18-2:20),StringStart(2:21-2:22),StringPart(2:22-2:28),StringEnd(2:28-2:29),Newline(1:1-1:1),
OpenRound(3:5-3:6),LowerIdent(3:6-3:7),Comma(3:7-3:8),UpperIdent(3:9-3:13),CloseRound(3:13-3:14),OpFatArrow(3:15-3:17),LowerIdent(3:18-3:19),Newline(1:1-1:1),
OpenRound(4:5-4:6),UpperIdent(4:6-4:10),Comma(4:10-4:11),LowerIdent(4:12-4:13),CloseRound(4:13-4:14),OpFatArrow(4:15-4:17),LowerIdent(4:18-4:19),Newline(1:1-1:1),
OpenRound(5:5-5:6),LowerIdent(5:6-5:7),Comma(5:7-5:8),LowerIdent(5:9-5:10),CloseRound(5:10-5:11),OpFatArrow(5:12-5:14),LowerIdent(5:15-5:16),Newline(1:1-1:1),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.12 (raw "coord"))
	(branches
		(branch @1.1-1.1
			(p-tuple @2.5-2.17
				(p-tag @2.6-2.10 (raw "Zero"))
				(p-tag @2.12-2.16 (raw "Zero")))
			(e-string @2.21-2.29
				(e-string-part @2.22-2.28 (raw "origin"))))
		(branch @3.5-4.6
			(p-tuple @3.5-3.14
				(p-ident @3.6-3.7 (raw "x"))
				(p-tag @3.9-3.13 (raw "Zero")))
			(e-ident @3.18-3.19 (raw "x")))
		(branch @4.5-5.6
			(p-tuple @4.5-4.14
				(p-tag @4.6-4.10 (raw "Zero"))
				(p-ident @4.12-4.13 (raw "y")))
			(e-ident @4.18-4.19 (raw "y")))
		(branch @5.5-6.2
			(p-tuple @5.5-5.11
				(p-ident @5.6-5.7 (raw "x"))
				(p-ident @5.9-5.10 (raw "y")))
			(e-ident @5.15-5.16 (raw "x")))))
~~~
# FORMATTED
~~~roc
match coord {
	(Zero, Zero) => "origin"
	(x, Zero) => x
	(Zero, y) => y
	(x, y) => x
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-6.2
	(match @1.1-6.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(p-tuple @2.5-2.17 (degenerate false)
						(patterns
							(p-applied-tag @2.6-2.10)
							(p-applied-tag @2.12-2.16))))
				(value
					(e-string @2.21-2.29
						(e-literal @2.22-2.28 (string "origin")))))
			(branch
				(patterns
					(p-tuple @3.5-3.14 (degenerate false)
						(patterns
							(p-assign @3.6-3.7 (ident "x"))
							(p-applied-tag @3.9-3.13))))
				(value
					(e-lookup-local @3.18-3.19
						(pattern @3.6-3.7))))
			(branch
				(patterns
					(p-tuple @4.5-4.14 (degenerate false)
						(patterns
							(p-applied-tag @4.6-4.10)
							(p-assign @4.12-4.13 (ident "y")))))
				(value
					(e-lookup-local @4.18-4.19
						(pattern @4.12-4.13))))
			(branch
				(patterns
					(p-tuple @5.5-5.11 (degenerate false)
						(patterns
							(p-assign @5.6-5.7 (ident "x"))
							(p-assign @5.9-5.10 (ident "y")))))
				(value
					(e-lookup-local @5.15-5.16
						(pattern @5.6-5.7)))))))
~~~
# TYPES
~~~clojure
(expr @1.1-6.2 (type "Str"))
~~~
