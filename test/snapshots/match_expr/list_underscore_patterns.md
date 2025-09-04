# META
~~~ini
description=Match expression with underscore patterns in list matching
type=expr
~~~
# SOURCE
~~~roc
match items {
    [_] => 1 # pattern match on a list with a single (ignored) element
    [.., last] => last # pattern match on the last item in the list
    [first, ..] => first # pattern match on the first item in the list
    [_, _, third] => third # pattern match on the third item in the list
    [x, _, _, y] => x + y # first + fourth item in the list
    [] => 0 # match an empty list
}
~~~
# EXPECTED
UNDEFINED VARIABLE - list_underscore_patterns.md:1:7:1:12
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `items` in this scope.
Is there an `import` or `exposing` missing up-top?

**list_underscore_patterns.md:1:7:1:12:**
```roc
match items {
```
      ^^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:12),OpenCurly(1:13-1:14),
OpenSquare(2:5-2:6),Underscore(2:6-2:7),CloseSquare(2:7-2:8),OpFatArrow(2:9-2:11),Int(2:12-2:13),
OpenSquare(3:5-3:6),DoubleDot(3:6-3:8),Comma(3:8-3:9),LowerIdent(3:10-3:14),CloseSquare(3:14-3:15),OpFatArrow(3:16-3:18),LowerIdent(3:19-3:23),
OpenSquare(4:5-4:6),LowerIdent(4:6-4:11),Comma(4:11-4:12),DoubleDot(4:13-4:15),CloseSquare(4:15-4:16),OpFatArrow(4:17-4:19),LowerIdent(4:20-4:25),
OpenSquare(5:5-5:6),Underscore(5:6-5:7),Comma(5:7-5:8),Underscore(5:9-5:10),Comma(5:10-5:11),LowerIdent(5:12-5:17),CloseSquare(5:17-5:18),OpFatArrow(5:19-5:21),LowerIdent(5:22-5:27),
OpenSquare(6:5-6:6),LowerIdent(6:6-6:7),Comma(6:7-6:8),Underscore(6:9-6:10),Comma(6:10-6:11),Underscore(6:12-6:13),Comma(6:13-6:14),LowerIdent(6:15-6:16),CloseSquare(6:16-6:17),OpFatArrow(6:18-6:20),LowerIdent(6:21-6:22),OpPlus(6:23-6:24),LowerIdent(6:25-6:26),
OpenSquare(7:5-7:6),CloseSquare(7:6-7:7),OpFatArrow(7:8-7:10),Int(7:11-7:12),
CloseCurly(8:1-8:2),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.12 (raw "items"))
	(branches
		(branch @2.5-2.13
			(p-list @2.5-2.8
				(p-underscore))
			(e-int @2.12-2.13 (raw "1")))
		(branch @3.5-3.23
			(p-list @3.5-3.15
				(p-list-rest @3.6-3.8)
				(p-ident @3.10-3.14 (raw "last")))
			(e-ident @3.19-3.23 (raw "last")))
		(branch @4.5-4.25
			(p-list @4.5-4.16
				(p-ident @4.6-4.11 (raw "first"))
				(p-list-rest @4.13-4.15))
			(e-ident @4.20-4.25 (raw "first")))
		(branch @5.5-5.27
			(p-list @5.5-5.18
				(p-underscore)
				(p-underscore)
				(p-ident @5.12-5.17 (raw "third")))
			(e-ident @5.22-5.27 (raw "third")))
		(branch @6.5-6.26
			(p-list @6.5-6.17
				(p-ident @6.6-6.7 (raw "x"))
				(p-underscore)
				(p-underscore)
				(p-ident @6.15-6.16 (raw "y")))
			(e-binop @6.21-6.26 (op "+")
				(e-ident @6.21-6.22 (raw "x"))
				(e-ident @6.25-6.26 (raw "y"))))
		(branch @7.5-7.12
			(p-list @7.5-7.7)
			(e-int @7.11-7.12 (raw "0")))))
~~~
# FORMATTED
~~~roc
match items {
	[_] => 1 # pattern match on a list with a single (ignored) element
	[.., last] => last # pattern match on the last item in the list
	[first, ..] => first # pattern match on the first item in the list
	[_, _, third] => third # pattern match on the third item in the list
	[x, _, _, y] => x + y # first + fourth item in the list
	[] => 0
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-8.2
	(match @1.1-8.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @2.5-2.8
							(patterns
								(p-underscore @2.6-2.7)))))
				(value
					(e-int @2.12-2.13 (value "1"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @3.5-3.15
							(patterns
								(p-assign @3.10-3.14 (ident "last")))
							(rest-at (index 0)))))
				(value
					(e-lookup-local @3.19-3.23
						(p-assign @3.10-3.14 (ident "last")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @4.5-4.16
							(patterns
								(p-assign @4.6-4.11 (ident "first")))
							(rest-at (index 1)))))
				(value
					(e-lookup-local @4.20-4.25
						(p-assign @4.6-4.11 (ident "first")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @5.5-5.18
							(patterns
								(p-underscore @5.6-5.7)
								(p-underscore @5.9-5.10)
								(p-assign @5.12-5.17 (ident "third"))))))
				(value
					(e-lookup-local @5.22-5.27
						(p-assign @5.12-5.17 (ident "third")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @6.5-6.17
							(patterns
								(p-assign @6.6-6.7 (ident "x"))
								(p-underscore @6.9-6.10)
								(p-underscore @6.12-6.13)
								(p-assign @6.15-6.16 (ident "y"))))))
				(value
					(e-binop @6.21-6.26 (op "add")
						(e-lookup-local @6.21-6.22
							(p-assign @6.6-6.7 (ident "x")))
						(e-lookup-local @6.25-6.26
							(p-assign @6.15-6.16 (ident "y"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @7.5-7.7
							(patterns))))
				(value
					(e-int @7.11-7.12 (value "0")))))))
~~~
# TYPES
~~~clojure
(expr @1.1-8.2 (type "Num(_size)"))
~~~
