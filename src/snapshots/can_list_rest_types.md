# META
~~~ini
description=List rest patterns should have correct list types matching element types
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [first, .. as restNums] => restNums
    [] => []
}
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `numbers` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: full list rest pattern matching
Let us know if you want to help!

# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:14),OpenCurly(1:15-1:16),Newline(1:1-1:1),
OpenSquare(2:5-2:6),LowerIdent(2:6-2:11),Comma(2:11-2:12),DoubleDot(2:13-2:15),KwAs(2:16-2:18),LowerIdent(2:19-2:27),CloseSquare(2:27-2:28),OpFatArrow(2:29-2:31),LowerIdent(2:32-2:40),Newline(1:1-1:1),
OpenSquare(3:5-3:6),CloseSquare(3:6-3:7),OpFatArrow(3:8-3:10),OpenSquare(3:11-3:12),CloseSquare(3:12-3:13),Newline(1:1-1:1),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.14 (qaul "") (raw "numbers"))
	(branches
		(branch @2.5-3.6
			(p-list @2.5-2.28
				(p-ident @2.6-2.11 (raw "first"))
				(p-list-rest @2.13-2.27 (name "restNums")))
			(e-ident @2.32-2.40 (qaul "") (raw "restNums")))
		(branch @3.5-4.2
			(p-list @3.5-3.7)
			(e-list @3.11-3.13))))
~~~
# FORMATTED
~~~roc
match numbers {
	[first, .. as restNums] => restNums
	[] => []
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-4.2
	(match @1.1-4.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(p-list @2.5-2.28 (degenerate false)
						(patterns
							(p-assign @2.6-2.11 (ident "first"))
							(p-assign @2.13-2.27 (ident "restNums")))))
				(value
					(e-lookup-local @2.32-2.40
						(pattern @2.13-2.27))))
			(branch
				(patterns
					(p-list @3.5-3.7 (degenerate false)
						(patterns)))
				(value
					(e-empty_list @3.11-3.13))))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "*"))
~~~
