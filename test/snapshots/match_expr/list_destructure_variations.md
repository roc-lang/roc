# META
~~~ini
description=Match expression with various list destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match list {
    [] => 0
    [x] => x
    [first, second] => first + second
    [head, .. as tail] => head
    [One, Two, .. as rest] => 3
    [x, y, z, .. as more] => x + y + z
}
~~~
# EXPECTED
UNDEFINED VARIABLE - list_destructure_variations.md:1:7:1:11
UNUSED VARIABLE - list_destructure_variations.md:1:1:1:1
UNUSED VARIABLE - list_destructure_variations.md:1:1:1:1
UNUSED VARIABLE - list_destructure_variations.md:1:1:1:1
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `list` in this scope.
Is there an `import` or `exposing` missing up-top?

**list_destructure_variations.md:1:7:1:11:**
```roc
match list {
```
      ^^^^


**UNUSED VARIABLE**
Variable `tail` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_tail` to suppress this warning.
The unused variable is declared here:
**list_destructure_variations.md:1:1:1:1:**
```roc
match list {
```



**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**list_destructure_variations.md:1:1:1:1:**
```roc
match list {
```



**UNUSED VARIABLE**
Variable `more` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_more` to suppress this warning.
The unused variable is declared here:
**list_destructure_variations.md:1:1:1:1:**
```roc
match list {
```



# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:11),OpenCurly(1:12-1:13),
OpenSquare(2:5-2:6),CloseSquare(2:6-2:7),OpFatArrow(2:8-2:10),Int(2:11-2:12),
OpenSquare(3:5-3:6),LowerIdent(3:6-3:7),CloseSquare(3:7-3:8),OpFatArrow(3:9-3:11),LowerIdent(3:12-3:13),
OpenSquare(4:5-4:6),LowerIdent(4:6-4:11),Comma(4:11-4:12),LowerIdent(4:13-4:19),CloseSquare(4:19-4:20),OpFatArrow(4:21-4:23),LowerIdent(4:24-4:29),OpPlus(4:30-4:31),LowerIdent(4:32-4:38),
OpenSquare(5:5-5:6),LowerIdent(5:6-5:10),Comma(5:10-5:11),DoubleDot(5:12-5:14),KwAs(5:15-5:17),LowerIdent(5:18-5:22),CloseSquare(5:22-5:23),OpFatArrow(5:24-5:26),LowerIdent(5:27-5:31),
OpenSquare(6:5-6:6),UpperIdent(6:6-6:9),Comma(6:9-6:10),UpperIdent(6:11-6:14),Comma(6:14-6:15),DoubleDot(6:16-6:18),KwAs(6:19-6:21),LowerIdent(6:22-6:26),CloseSquare(6:26-6:27),OpFatArrow(6:28-6:30),Int(6:31-6:32),
OpenSquare(7:5-7:6),LowerIdent(7:6-7:7),Comma(7:7-7:8),LowerIdent(7:9-7:10),Comma(7:10-7:11),LowerIdent(7:12-7:13),Comma(7:13-7:14),DoubleDot(7:15-7:17),KwAs(7:18-7:20),LowerIdent(7:21-7:25),CloseSquare(7:25-7:26),OpFatArrow(7:27-7:29),LowerIdent(7:30-7:31),OpPlus(7:32-7:33),LowerIdent(7:34-7:35),OpPlus(7:36-7:37),LowerIdent(7:38-7:39),
CloseCurly(8:1-8:2),EndOfFile(8:2-8:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.11 (raw "list"))
	(branches
		(branch @2.5-2.12
			(p-list @2.5-2.7)
			(e-int @2.11-2.12 (raw "0")))
		(branch @3.5-3.13
			(p-list @3.5-3.8
				(p-ident @3.6-3.7 (raw "x")))
			(e-ident @3.12-3.13 (raw "x")))
		(branch @4.5-4.38
			(p-list @4.5-4.20
				(p-ident @4.6-4.11 (raw "first"))
				(p-ident @4.13-4.19 (raw "second")))
			(e-binop @4.24-4.38 (op "+")
				(e-ident @4.24-4.29 (raw "first"))
				(e-ident @4.32-4.38 (raw "second"))))
		(branch @5.5-5.31
			(p-list @5.5-5.23
				(p-ident @5.6-5.10 (raw "head"))
				(p-list-rest @5.12-5.22 (name "tail")))
			(e-ident @5.27-5.31 (raw "head")))
		(branch @6.5-6.32
			(p-list @6.5-6.27
				(p-tag @6.6-6.9 (raw "One"))
				(p-tag @6.11-6.14 (raw "Two"))
				(p-list-rest @6.16-6.26 (name "rest")))
			(e-int @6.31-6.32 (raw "3")))
		(branch @7.5-7.39
			(p-list @7.5-7.26
				(p-ident @7.6-7.7 (raw "x"))
				(p-ident @7.9-7.10 (raw "y"))
				(p-ident @7.12-7.13 (raw "z"))
				(p-list-rest @7.15-7.25 (name "more")))
			(e-binop @7.30-7.39 (op "+")
				(e-binop @7.30-7.35 (op "+")
					(e-ident @7.30-7.31 (raw "x"))
					(e-ident @7.34-7.35 (raw "y")))
				(e-ident @7.38-7.39 (raw "z"))))))
~~~
# FORMATTED
~~~roc
match list {
	[] => 0
	[x] => x
	[first, second] => first + second
	[head, .. as tail] => head
	[One, Two, .. as rest] => 3
	[x, y, z, .. as more] => x + y + z
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
						(p-list @2.5-2.7
							(patterns))))
				(value
					(e-int @2.11-2.12 (value "0"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @3.5-3.8
							(patterns
								(p-assign @3.6-3.7 (ident "x"))))))
				(value
					(e-lookup-local @3.12-3.13
						(p-assign @3.6-3.7 (ident "x")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @4.5-4.20
							(patterns
								(p-assign @4.6-4.11 (ident "first"))
								(p-assign @4.13-4.19 (ident "second"))))))
				(value
					(e-binop @4.24-4.38 (op "add")
						(e-lookup-local @4.24-4.29
							(p-assign @4.6-4.11 (ident "first")))
						(e-lookup-local @4.32-4.38
							(p-assign @4.13-4.19 (ident "second"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @5.5-5.23
							(patterns
								(p-assign @5.6-5.10 (ident "head")))
							(rest-at (index 1)
								(p-assign @1.1-1.1 (ident "tail"))))))
				(value
					(e-lookup-local @5.27-5.31
						(p-assign @5.6-5.10 (ident "head")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @6.5-6.27
							(patterns
								(p-applied-tag @6.6-6.9)
								(p-applied-tag @6.11-6.14))
							(rest-at (index 2)
								(p-assign @1.1-1.1 (ident "rest"))))))
				(value
					(e-int @6.31-6.32 (value "3"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @7.5-7.26
							(patterns
								(p-assign @7.6-7.7 (ident "x"))
								(p-assign @7.9-7.10 (ident "y"))
								(p-assign @7.12-7.13 (ident "z")))
							(rest-at (index 3)
								(p-assign @1.1-1.1 (ident "more"))))))
				(value
					(e-binop @7.30-7.39 (op "add")
						(e-binop @7.30-7.35 (op "add")
							(e-lookup-local @7.30-7.31
								(p-assign @7.6-7.7 (ident "x")))
							(e-lookup-local @7.34-7.35
								(p-assign @7.9-7.10 (ident "y"))))
						(e-lookup-local @7.38-7.39
							(p-assign @7.12-7.13 (ident "z")))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-8.2 (type "Num(_size)"))
~~~
