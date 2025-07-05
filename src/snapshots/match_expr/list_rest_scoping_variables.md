# META
~~~ini
description=List rest patterns with proper variable scoping across branches
type=expr
~~~
# SOURCE
~~~roc
match data {
    [..items] => 1
    [first, ..items] => first
    [..items, last] => last
    [first, ..items, last] => first + last
}
~~~
# EXPECTED
BAD LIST REST PATTERN SYNTAX - list_rest_scoping_variables.md:2:6:2:13
BAD LIST REST PATTERN SYNTAX - list_rest_scoping_variables.md:3:13:3:20
BAD LIST REST PATTERN SYNTAX - list_rest_scoping_variables.md:4:6:4:13
BAD LIST REST PATTERN SYNTAX - list_rest_scoping_variables.md:5:13:5:20
UNDEFINED VARIABLE - list_rest_scoping_variables.md:1:7:1:11
UNUSED VARIABLE - list_rest_scoping_variables.md:2:8:2:13
UNUSED VARIABLE - list_rest_scoping_variables.md:3:15:3:20
UNUSED VARIABLE - list_rest_scoping_variables.md:4:8:4:13
UNUSED VARIABLE - list_rest_scoping_variables.md:5:15:5:20
# PROBLEMS
**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

Here is the problematic code:
**list_rest_scoping_variables.md:2:6:2:13:**
```roc
    [..items] => 1
```
     ^^^^^^^


**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

Here is the problematic code:
**list_rest_scoping_variables.md:3:13:3:20:**
```roc
    [first, ..items] => first
```
            ^^^^^^^


**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

Here is the problematic code:
**list_rest_scoping_variables.md:4:6:4:13:**
```roc
    [..items, last] => last
```
     ^^^^^^^


**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

Here is the problematic code:
**list_rest_scoping_variables.md:5:13:5:20:**
```roc
    [first, ..items, last] => first + last
```
            ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `data` in this scope.
Is there an `import` or `exposing` missing up-top?

**list_rest_scoping_variables.md:1:7:1:11:**
```roc
match data {
```
      ^^^^


**UNUSED VARIABLE**
Variable ``items`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_items` to suppress this warning.
The unused variable is declared here:
**list_rest_scoping_variables.md:2:8:2:13:**
```roc
    [..items] => 1
```
       ^^^^^


**UNUSED VARIABLE**
Variable ``items`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_items` to suppress this warning.
The unused variable is declared here:
**list_rest_scoping_variables.md:3:15:3:20:**
```roc
    [first, ..items] => first
```
              ^^^^^


**UNUSED VARIABLE**
Variable ``items`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_items` to suppress this warning.
The unused variable is declared here:
**list_rest_scoping_variables.md:4:8:4:13:**
```roc
    [..items, last] => last
```
       ^^^^^


**UNUSED VARIABLE**
Variable ``items`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_items` to suppress this warning.
The unused variable is declared here:
**list_rest_scoping_variables.md:5:15:5:20:**
```roc
    [first, ..items, last] => first + last
```
              ^^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:11),OpenCurly(1:12-1:13),Newline(1:1-1:1),
OpenSquare(2:5-2:6),DoubleDot(2:6-2:8),LowerIdent(2:8-2:13),CloseSquare(2:13-2:14),OpFatArrow(2:15-2:17),Int(2:18-2:19),Newline(1:1-1:1),
OpenSquare(3:5-3:6),LowerIdent(3:6-3:11),Comma(3:11-3:12),DoubleDot(3:13-3:15),LowerIdent(3:15-3:20),CloseSquare(3:20-3:21),OpFatArrow(3:22-3:24),LowerIdent(3:25-3:30),Newline(1:1-1:1),
OpenSquare(4:5-4:6),DoubleDot(4:6-4:8),LowerIdent(4:8-4:13),Comma(4:13-4:14),LowerIdent(4:15-4:19),CloseSquare(4:19-4:20),OpFatArrow(4:21-4:23),LowerIdent(4:24-4:28),Newline(1:1-1:1),
OpenSquare(5:5-5:6),LowerIdent(5:6-5:11),Comma(5:11-5:12),DoubleDot(5:13-5:15),LowerIdent(5:15-5:20),Comma(5:20-5:21),LowerIdent(5:22-5:26),CloseSquare(5:26-5:27),OpFatArrow(5:28-5:30),LowerIdent(5:31-5:36),OpPlus(5:37-5:38),LowerIdent(5:39-5:43),Newline(1:1-1:1),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.11 (raw "data"))
	(branches
		(branch @2.5-3.6
			(p-list @2.5-2.14
				(p-list-rest @2.6-2.14 (name "items")))
			(e-int @2.18-2.19 (raw "1")))
		(branch @3.5-4.6
			(p-list @3.5-3.21
				(p-ident @3.6-3.11 (raw "first"))
				(p-list-rest @3.13-3.21 (name "items")))
			(e-ident @3.25-3.30 (raw "first")))
		(branch @4.5-5.6
			(p-list @4.5-4.20
				(p-list-rest @4.6-4.14 (name "items"))
				(p-ident @4.15-4.19 (raw "last")))
			(e-ident @4.24-4.28 (raw "last")))
		(branch @5.5-6.2
			(p-list @5.5-5.27
				(p-ident @5.6-5.11 (raw "first"))
				(p-list-rest @5.13-5.21 (name "items"))
				(p-ident @5.22-5.26 (raw "last")))
			(e-binop @5.31-6.2 (op "+")
				(e-ident @5.31-5.36 (raw "first"))
				(e-ident @5.39-5.43 (raw "last"))))))
~~~
# FORMATTED
~~~roc
match data {
	[.. as items] => 1
	[first, .. as items] => first
	[.. as items, last] => last
	[first, .. as items, last] => first + last
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
					(p-list @2.5-2.14 (degenerate false)
						(patterns)
						(rest-at (index 0)
							(p-assign @2.8-2.13 (ident "items")))))
				(value
					(e-int @2.18-2.19 (value "1"))))
			(branch
				(patterns
					(p-list @3.5-3.21 (degenerate false)
						(patterns
							(p-assign @3.6-3.11 (ident "first")))
						(rest-at (index 1)
							(p-assign @3.15-3.20 (ident "items")))))
				(value
					(e-lookup-local @3.25-3.30
						(pattern @3.6-3.11))))
			(branch
				(patterns
					(p-list @4.5-4.20 (degenerate false)
						(patterns
							(p-assign @4.15-4.19 (ident "last")))
						(rest-at (index 0)
							(p-assign @4.8-4.13 (ident "items")))))
				(value
					(e-lookup-local @4.24-4.28
						(pattern @4.15-4.19))))
			(branch
				(patterns
					(p-list @5.5-5.27 (degenerate false)
						(patterns
							(p-assign @5.6-5.11 (ident "first"))
							(p-assign @5.22-5.26 (ident "last")))
						(rest-at (index 1)
							(p-assign @5.15-5.20 (ident "items")))))
				(value
					(e-binop @5.31-6.2 (op "add")
						(e-lookup-local @5.31-5.36
							(pattern @5.6-5.11))
						(e-lookup-local @5.39-5.43
							(pattern @5.22-5.26))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-6.2 (type "Num(*)"))
~~~
