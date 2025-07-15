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
# EXPECTED
UNDEFINED VARIABLE - can_list_rest_types.md:1:7:1:14
UNUSED VARIABLE - can_list_rest_types.md:2:6:2:11
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `numbers` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_list_rest_types.md:1:7:1:14:**
```roc
match numbers {
```
      ^^^^^^^


**UNUSED VARIABLE**
Variable `first` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_first to suppress this warning.
The unused variable is declared here:
**can_list_rest_types.md:2:6:2:11:**
```roc
    [first, .. as restNums] => restNums
```
     ^^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:14),OpenCurly(1:15-1:16),
OpenSquare(2:5-2:6),LowerIdent(2:6-2:11),Comma(2:11-2:12),DoubleDot(2:13-2:15),KwAs(2:16-2:18),LowerIdent(2:19-2:27),CloseSquare(2:27-2:28),OpFatArrow(2:29-2:31),LowerIdent(2:32-2:40),
OpenSquare(3:5-3:6),CloseSquare(3:6-3:7),OpFatArrow(3:8-3:10),OpenSquare(3:11-3:12),CloseSquare(3:12-3:13),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.14 (raw "numbers"))
	(branches
		(branch @2.5-2.40
			(p-list @2.5-2.28
				(p-ident @2.6-2.11 (raw "first"))
				(p-list-rest @2.13-2.27 (name "restNums")))
			(e-ident @2.32-2.40 (raw "restNums")))
		(branch @3.5-3.13
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
					(pattern (degenerate false)
						(p-list @2.5-2.28
							(patterns
								(p-assign @2.6-2.11 (ident "first")))
							(rest-at (index 1)
								(p-assign @1.1-1.1 (ident "restNums"))))))
				(value
					(e-lookup-local @2.32-2.40
						(p-assign @1.1-1.1 (ident "restNums")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @3.5-3.7
							(patterns))))
				(value
					(e-empty_list @3.11-3.13))))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "List(_elem)"))
~~~
