# META
~~~ini
description=Match expression with various list destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match list {
    [first] => first
    [first, second] => first + second
}
~~~
# EXPECTED
UNDEFINED VARIABLE - list_destructure_scoping.md:1:7:1:11
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `list` in this scope.
Is there an `import` or `exposing` missing up-top?

**list_destructure_scoping.md:1:7:1:11:**
```roc
match list {
```
      ^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:11),OpenCurly(1:12-1:13),
OpenSquare(2:5-2:6),LowerIdent(2:6-2:11),CloseSquare(2:11-2:12),OpFatArrow(2:13-2:15),LowerIdent(2:16-2:21),
OpenSquare(3:5-3:6),LowerIdent(3:6-3:11),Comma(3:11-3:12),LowerIdent(3:13-3:19),CloseSquare(3:19-3:20),OpFatArrow(3:21-3:23),LowerIdent(3:24-3:29),OpPlus(3:30-3:31),LowerIdent(3:32-3:38),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.11 (raw "list"))
	(branches
		(branch @2.5-2.21
			(p-list @2.5-2.12
				(p-ident @2.6-2.11 (raw "first")))
			(e-ident @2.16-2.21 (raw "first")))
		(branch @3.5-3.38
			(p-list @3.5-3.20
				(p-ident @3.6-3.11 (raw "first"))
				(p-ident @3.13-3.19 (raw "second")))
			(e-binop @3.24-3.38 (op "+")
				(e-ident @3.24-3.29 (raw "first"))
				(e-ident @3.32-3.38 (raw "second"))))))
~~~
# FORMATTED
~~~roc
match list {
	[first] => first
	[first, second] => first + second
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
						(p-list @2.5-2.12
							(patterns
								(p-assign @2.6-2.11 (ident "first"))))))
				(value
					(e-lookup-local @2.16-2.21
						(p-assign @2.6-2.11 (ident "first")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @3.5-3.20
							(patterns
								(p-assign @3.6-3.11 (ident "first"))
								(p-assign @3.13-3.19 (ident "second"))))))
				(value
					(e-binop @3.24-3.38 (op "add")
						(e-lookup-local @3.24-3.29
							(p-assign @3.6-3.11 (ident "first")))
						(e-lookup-local @3.32-3.38
							(p-assign @3.13-3.19 (ident "second")))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "Num(_size)"))
~~~
