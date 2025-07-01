# META
~~~ini
description=Match expression with list patterns including empty list and rest patterns
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [] => acc
    [first, .. as rest] => first + acc
}
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `numbers` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `acc` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `acc` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNUSED VARIABLE**
Variable ``rest`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**list_patterns.md:3:13:3:23:**
```roc
    [first, .. as rest] => first + acc
```
            ^^^^^^^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:14),OpenCurly(1:15-1:16),Newline(1:1-1:1),
OpenSquare(2:5-2:6),CloseSquare(2:6-2:7),OpFatArrow(2:8-2:10),LowerIdent(2:11-2:14),Newline(1:1-1:1),
OpenSquare(3:5-3:6),LowerIdent(3:6-3:11),Comma(3:11-3:12),DoubleDot(3:13-3:15),KwAs(3:16-3:18),LowerIdent(3:19-3:23),CloseSquare(3:23-3:24),OpFatArrow(3:25-3:27),LowerIdent(3:28-3:33),OpPlus(3:34-3:35),LowerIdent(3:36-3:39),Newline(1:1-1:1),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.14 (qaul "") (raw "numbers"))
	(branches
		(branch @2.5-3.6
			(p-list @2.5-2.7)
			(e-ident @2.11-2.14 (qaul "") (raw "acc")))
		(branch @3.5-4.2
			(p-list @3.5-3.24
				(p-ident @3.6-3.11 (raw "first"))
				(p-list-rest @3.13-3.23 (name "rest")))
			(e-binop @3.28-4.2 (op "+")
				(e-ident @3.28-3.33 (qaul "") (raw "first"))
				(e-ident @3.36-3.39 (qaul "") (raw "acc"))))))
~~~
# FORMATTED
~~~roc
match numbers {
	[] => acc
	[first, .. as rest] => first + acc
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
					(p-list @2.5-2.7 (degenerate false)
						(patterns)))
				(value
					(e-runtime-error (tag "ident_not_in_scope"))))
			(branch
				(patterns
					(p-list @3.5-3.24 (degenerate false)
						(patterns
							(p-assign @3.6-3.11 (ident "first")))
						(rest-at (index 1)
							(p-assign @3.13-3.23 (ident "rest")))))
				(value
					(e-binop @3.28-4.2 (op "add")
						(e-lookup-local @3.28-3.33
							(pattern @3.6-3.11))
						(e-runtime-error (tag "ident_not_in_scope"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "*"))
~~~
