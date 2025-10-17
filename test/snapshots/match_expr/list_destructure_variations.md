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
^


**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**list_destructure_variations.md:1:1:1:1:**
```roc
match list {
```
^


**UNUSED VARIABLE**
Variable `more` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_more` to suppress this warning.
The unused variable is declared here:
**list_destructure_variations.md:1:1:1:1:**
```roc
match list {
```
^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,CloseSquare,OpFatArrow,Int,
OpenSquare,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
OpenSquare,LowerIdent,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,UpperIdent,Comma,UpperIdent,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,Int,
OpenSquare,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "list"))
	(branches
		(branch
			(p-list)
			(e-int (raw "0")))
		(branch
			(p-list
				(p-ident (raw "x")))
			(e-ident (raw "x")))
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-ident (raw "second")))
			(e-binop (op "+")
				(e-ident (raw "first"))
				(e-ident (raw "second"))))
		(branch
			(p-list
				(p-ident (raw "head"))
				(p-list-rest (name "tail")))
			(e-ident (raw "head")))
		(branch
			(p-list
				(p-tag (raw "One"))
				(p-tag (raw "Two"))
				(p-list-rest (name "rest")))
			(e-int (raw "3")))
		(branch
			(p-list
				(p-ident (raw "x"))
				(p-ident (raw "y"))
				(p-ident (raw "z"))
				(p-list-rest (name "more")))
			(e-binop (op "+")
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-ident (raw "y")))
				(e-ident (raw "z"))))))
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
(e-match
	(match
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns))))
				(value
					(e-num (value "0"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "x"))))))
				(value
					(e-lookup-local
						(p-assign (ident "x")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "first"))
								(p-assign (ident "second"))))))
				(value
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "first")))
						(e-lookup-local
							(p-assign (ident "second"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "head")))
							(rest-at (index 1)
								(p-assign (ident "tail"))))))
				(value
					(e-lookup-local
						(p-assign (ident "head")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-applied-tag)
								(p-applied-tag))
							(rest-at (index 2)
								(p-assign (ident "rest"))))))
				(value
					(e-num (value "3"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "x"))
								(p-assign (ident "y"))
								(p-assign (ident "z")))
							(rest-at (index 3)
								(p-assign (ident "more"))))))
				(value
					(e-binop (op "add")
						(e-binop (op "add")
							(e-lookup-local
								(p-assign (ident "x")))
							(e-lookup-local
								(p-assign (ident "y"))))
						(e-lookup-local
							(p-assign (ident "z")))))))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
