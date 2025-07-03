# META
~~~ini
description=Match expression with list rest patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match items {
    [first, ..rest] => first + 1
    [..rest, last] => last + 2
    [x, ..rest, y] => x + y
}
~~~
# PROBLEMS
**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

Here is the problematic code:
**list_rest_scoping.md:2:13:2:19:**
```roc
    [first, ..rest] => first + 1
```
            ^^^^^^


**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

Here is the problematic code:
**list_rest_scoping.md:3:6:3:12:**
```roc
    [..rest, last] => last + 2
```
     ^^^^^^


**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

Here is the problematic code:
**list_rest_scoping.md:4:9:4:15:**
```roc
    [x, ..rest, y] => x + y
```
        ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `items` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNUSED VARIABLE**
Variable ``rest`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**list_rest_scoping.md:2:15:2:19:**
```roc
    [first, ..rest] => first + 1
```
              ^^^^


**UNUSED VARIABLE**
Variable ``rest`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**list_rest_scoping.md:3:8:3:12:**
```roc
    [..rest, last] => last + 2
```
       ^^^^


**UNUSED VARIABLE**
Variable ``rest`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**list_rest_scoping.md:4:11:4:15:**
```roc
    [x, ..rest, y] => x + y
```
          ^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:12),OpenCurly(1:13-1:14),Newline(1:1-1:1),
OpenSquare(2:5-2:6),LowerIdent(2:6-2:11),Comma(2:11-2:12),DoubleDot(2:13-2:15),LowerIdent(2:15-2:19),CloseSquare(2:19-2:20),OpFatArrow(2:21-2:23),LowerIdent(2:24-2:29),OpPlus(2:30-2:31),Int(2:32-2:33),Newline(1:1-1:1),
OpenSquare(3:5-3:6),DoubleDot(3:6-3:8),LowerIdent(3:8-3:12),Comma(3:12-3:13),LowerIdent(3:14-3:18),CloseSquare(3:18-3:19),OpFatArrow(3:20-3:22),LowerIdent(3:23-3:27),OpPlus(3:28-3:29),Int(3:30-3:31),Newline(1:1-1:1),
OpenSquare(4:5-4:6),LowerIdent(4:6-4:7),Comma(4:7-4:8),DoubleDot(4:9-4:11),LowerIdent(4:11-4:15),Comma(4:15-4:16),LowerIdent(4:17-4:18),CloseSquare(4:18-4:19),OpFatArrow(4:20-4:22),LowerIdent(4:23-4:24),OpPlus(4:25-4:26),LowerIdent(4:27-4:28),Newline(1:1-1:1),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.12 (qaul "") (raw "items"))
	(branches
		(branch @2.5-3.6
			(p-list @2.5-2.20
				(p-ident @2.6-2.11 (raw "first"))
				(p-list-rest @2.13-2.20 (name "rest")))
			(e-binop @2.24-3.6 (op "+")
				(e-ident @2.24-2.29 (qaul "") (raw "first"))
				(e-int @2.32-2.33 (raw "1"))))
		(branch @3.5-4.6
			(p-list @3.5-3.19
				(p-list-rest @3.6-3.13 (name "rest"))
				(p-ident @3.14-3.18 (raw "last")))
			(e-binop @3.23-4.6 (op "+")
				(e-ident @3.23-3.27 (qaul "") (raw "last"))
				(e-int @3.30-3.31 (raw "2"))))
		(branch @4.5-5.2
			(p-list @4.5-4.19
				(p-ident @4.6-4.7 (raw "x"))
				(p-list-rest @4.9-4.16 (name "rest"))
				(p-ident @4.17-4.18 (raw "y")))
			(e-binop @4.23-5.2 (op "+")
				(e-ident @4.23-4.24 (qaul "") (raw "x"))
				(e-ident @4.27-4.28 (qaul "") (raw "y"))))))
~~~
# FORMATTED
~~~roc
match items {
	[first, .. as rest] => first + 1
	[.. as rest, last] => last + 2
	[x, .. as rest, y] => x + y
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
					(p-list @2.5-2.20 (degenerate false)
						(patterns
							(p-assign @2.6-2.11 (ident "first")))
						(rest-at (index 1)
							(p-assign @2.15-2.19 (ident "rest")))))
				(value
					(e-binop @2.24-3.6 (op "add")
						(e-lookup-local @2.24-2.29
							(pattern @2.6-2.11))
						(e-int @2.32-2.33 (value "1")))))
			(branch
				(patterns
					(p-list @3.5-3.19 (degenerate false)
						(patterns
							(p-assign @3.14-3.18 (ident "last")))
						(rest-at (index 0)
							(p-assign @3.8-3.12 (ident "rest")))))
				(value
					(e-binop @3.23-4.6 (op "add")
						(e-lookup-local @3.23-3.27
							(pattern @3.14-3.18))
						(e-int @3.30-3.31 (value "2")))))
			(branch
				(patterns
					(p-list @4.5-4.19 (degenerate false)
						(patterns
							(p-assign @4.6-4.7 (ident "x"))
							(p-assign @4.17-4.18 (ident "y")))
						(rest-at (index 1)
							(p-assign @4.11-4.15 (ident "rest")))))
				(value
					(e-binop @4.23-5.2 (op "add")
						(e-lookup-local @4.23-4.24
							(pattern @4.6-4.7))
						(e-lookup-local @4.27-4.28
							(pattern @4.17-4.18))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "a"))
~~~
