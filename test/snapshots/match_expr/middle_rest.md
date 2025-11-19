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
UNUSED VARIABLE - middle_rest.md:1:1:1:1
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
Variable `middle` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_middle` to suppress this warning.
The unused variable is declared here:
**middle_rest.md:1:1:1:1:**
```roc
match items {
```
^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,Comma,DoubleDot,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
OpenSquare,LowerIdent,Comma,LowerIdent,Comma,DoubleDot,KwAs,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
OpenSquare,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,CloseSquare,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "items"))
	(branches
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-list-rest)
				(p-ident (raw "last")))
			(e-binop (op "+")
				(e-ident (raw "first"))
				(e-ident (raw "last"))))
		(branch
			(p-list
				(p-ident (raw "a"))
				(p-ident (raw "b"))
				(p-list-rest (name "middle"))
				(p-ident (raw "x"))
				(p-ident (raw "y")))
			(e-binop (op "+")
				(e-binop (op "+")
					(e-binop (op "+")
						(e-ident (raw "a"))
						(e-ident (raw "b")))
					(e-ident (raw "x")))
				(e-ident (raw "y"))))
		(branch
			(p-list
				(p-ident (raw "single")))
			(e-ident (raw "single")))
		(branch
			(p-list)
			(e-int (raw "0")))))
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
(e-match
	(match
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "first"))
								(p-assign (ident "last")))
							(rest-at (index 1)))))
				(value
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "first")))
						(e-lookup-local
							(p-assign (ident "last"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "a"))
								(p-assign (ident "b"))
								(p-assign (ident "x"))
								(p-assign (ident "y")))
							(rest-at (index 2)
								(p-assign (ident "middle"))))))
				(value
					(e-binop (op "add")
						(e-binop (op "add")
							(e-binop (op "add")
								(e-lookup-local
									(p-assign (ident "a")))
								(e-lookup-local
									(p-assign (ident "b"))))
							(e-lookup-local
								(p-assign (ident "x"))))
						(e-lookup-local
							(p-assign (ident "y"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "single"))))))
				(value
					(e-lookup-local
						(p-assign (ident "single")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns))))
				(value
					(e-num (value "0")))))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
