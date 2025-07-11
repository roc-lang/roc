# META
~~~ini
description=Match expression with rest patterns in middle position
type=expr
~~~
# SOURCE
~~~roc
match items {
    [first, .., last] => first + last
    [a, b, .. as middle, x, y] => a + b + x + y  
    [single] => single
    [] => 0
}
~~~
# EXPECTED
UNDEFINED VARIABLE - middle_rest.md:1:7:1:12
UNUSED VARIABLE - middle_rest.md:3:18:3:24
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `items` in this scope.
Is there an `import` or `exposing` missing up-top?

**middle_rest.md:1:7:1:12:**
```roc
match items {
```
      ^^^^^


**UNUSED VARIABLE**
Variable ``middle`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_middle` to suppress this warning.
The unused variable is declared here:
**middle_rest.md:3:18:3:24:**
```roc
    [a, b, .. as middle, x, y] => a + b + x + y  
```
                 ^^^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:12),OpenCurly(1:13-1:14),
OpenSquare(2:5-2:6),LowerIdent(2:6-2:11),Comma(2:11-2:12),DoubleDot(2:13-2:15),Comma(2:15-2:16),LowerIdent(2:17-2:21),CloseSquare(2:21-2:22),OpFatArrow(2:23-2:25),LowerIdent(2:26-2:31),OpPlus(2:32-2:33),LowerIdent(2:34-2:38),
OpenSquare(3:5-3:6),LowerIdent(3:6-3:7),Comma(3:7-3:8),LowerIdent(3:9-3:10),Comma(3:10-3:11),DoubleDot(3:12-3:14),KwAs(3:15-3:17),LowerIdent(3:18-3:24),Comma(3:24-3:25),LowerIdent(3:26-3:27),Comma(3:27-3:28),LowerIdent(3:29-3:30),CloseSquare(3:30-3:31),OpFatArrow(3:32-3:34),LowerIdent(3:35-3:36),OpPlus(3:37-3:38),LowerIdent(3:39-3:40),OpPlus(3:41-3:42),LowerIdent(3:43-3:44),OpPlus(3:45-3:46),LowerIdent(3:47-3:48),
OpenSquare(4:5-4:6),LowerIdent(4:6-4:12),CloseSquare(4:12-4:13),OpFatArrow(4:14-4:16),LowerIdent(4:17-4:23),
OpenSquare(5:5-5:6),CloseSquare(5:6-5:7),OpFatArrow(5:8-5:10),Int(5:11-5:12),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.12 (raw "items"))
	(branches
		(branch @2.5-3.6
			(p-list @2.5-2.22
				(p-ident @2.6-2.11 (raw "first"))
				(p-list-rest @2.13-2.16)
				(p-ident @2.17-2.21 (raw "last")))
			(e-binop @2.26-3.6 (op "+")
				(e-ident @2.26-2.31 (raw "first"))
				(e-ident @2.34-2.38 (raw "last"))))
		(branch @3.5-4.6
			(p-list @3.5-3.31
				(p-ident @3.6-3.7 (raw "a"))
				(p-ident @3.9-3.10 (raw "b"))
				(p-list-rest @3.12-3.25 (name "middle"))
				(p-ident @3.26-3.27 (raw "x"))
				(p-ident @3.29-3.30 (raw "y")))
			(e-binop @3.35-4.6 (op "+")
				(e-ident @3.35-3.36 (raw "a"))
				(e-binop @3.39-4.6 (op "+")
					(e-ident @3.39-3.40 (raw "b"))
					(e-binop @3.43-4.6 (op "+")
						(e-ident @3.43-3.44 (raw "x"))
						(e-ident @3.47-3.48 (raw "y"))))))
		(branch @4.5-5.6
			(p-list @4.5-4.13
				(p-ident @4.6-4.12 (raw "single")))
			(e-ident @4.17-4.23 (raw "single")))
		(branch @5.5-6.2
			(p-list @5.5-5.7)
			(e-int @5.11-5.12 (raw "0")))))
~~~
# FORMATTED
~~~roc
match items {
	[first, .., last] => first + last
	[a, b, .. as middle, x, y] => a + b + x + y
	[single] => single
	[] => 0
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
					(pattern (degenerate false)
						(p-list @2.5-2.22
							(patterns
								(p-assign @2.6-2.11 (ident "first"))
								(p-assign @2.17-2.21 (ident "last")))
							(rest-at (index 1)))))
				(value
					(e-binop @2.26-3.6 (op "add")
						(e-lookup-local @2.26-2.31
							(p-assign @2.6-2.11 (ident "first")))
						(e-lookup-local @2.34-2.38
							(p-assign @2.17-2.21 (ident "last"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @3.5-3.31
							(patterns
								(p-assign @3.6-3.7 (ident "a"))
								(p-assign @3.9-3.10 (ident "b"))
								(p-assign @3.26-3.27 (ident "x"))
								(p-assign @3.29-3.30 (ident "y")))
							(rest-at (index 2)
								(p-assign @3.18-3.24 (ident "middle"))))))
				(value
					(e-binop @3.35-4.6 (op "add")
						(e-lookup-local @3.35-3.36
							(p-assign @3.6-3.7 (ident "a")))
						(e-binop @3.39-4.6 (op "add")
							(e-lookup-local @3.39-3.40
								(p-assign @3.9-3.10 (ident "b")))
							(e-binop @3.43-4.6 (op "add")
								(e-lookup-local @3.43-3.44
									(p-assign @3.26-3.27 (ident "x")))
								(e-lookup-local @3.47-3.48
									(p-assign @3.29-3.30 (ident "y"))))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @4.5-4.13
							(patterns
								(p-assign @4.6-4.12 (ident "single"))))))
				(value
					(e-lookup-local @4.17-4.23
						(p-assign @4.6-4.12 (ident "single")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @5.5-5.7
							(patterns))))
				(value
					(e-int @5.11-5.12 (value "0")))))))
~~~
# TYPES
~~~clojure
(expr @1.1-6.2 (type "Num(*)"))
~~~
