# META
~~~ini
description=Match expression with nested list patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match nestedList {
    [[x], [y]] => x + y
    [[x, y]] => x - y  
    [x, [y]] => x * y
}
~~~
# EXPECTED
UNDEFINED VARIABLE - nested_list_scoping.md:1:7:1:17
TYPE MISMATCH - nested_list_scoping.md:4:17:4:18
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `nestedList` in this scope.
Is there an `import` or `exposing` missing up-top?

**nested_list_scoping.md:1:7:1:17:**
```roc
match nestedList {
```
      ^^^^^^^^^^


**TYPE MISMATCH**
This expression is used in an unexpected way:
**nested_list_scoping.md:4:17:4:18:**
```roc
    [x, [y]] => x * y
```
                ^

It has the type:
    _List(_elem)_

But I expected it to be:
    _Num(_size)_

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,OpenSquare,LowerIdent,CloseSquare,Comma,OpenSquare,LowerIdent,CloseSquare,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
OpenSquare,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,CloseSquare,OpFatArrow,LowerIdent,OpBinaryMinus,LowerIdent,
OpenSquare,LowerIdent,Comma,OpenSquare,LowerIdent,CloseSquare,CloseSquare,OpFatArrow,LowerIdent,OpStar,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "nestedList"))
	(branches
		(branch
			(p-list
				(p-list
					(p-ident (raw "x")))
				(p-list
					(p-ident (raw "y"))))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-ident (raw "y"))))
		(branch
			(p-list
				(p-list
					(p-ident (raw "x"))
					(p-ident (raw "y"))))
			(e-binop (op "-")
				(e-ident (raw "x"))
				(e-ident (raw "y"))))
		(branch
			(p-list
				(p-ident (raw "x"))
				(p-list
					(p-ident (raw "y"))))
			(e-binop (op "*")
				(e-ident (raw "x"))
				(e-ident (raw "y"))))))
~~~
# FORMATTED
~~~roc
match nestedList {
	[[x], [y]] => x + y
	[[x, y]] => x - y
	[x, [y]] => x * y
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
								(p-list
									(patterns
										(p-assign (ident "x"))))
								(p-list
									(patterns
										(p-assign (ident "y"))))))))
				(value
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "y"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-list
									(patterns
										(p-assign (ident "x"))
										(p-assign (ident "y"))))))))
				(value
					(e-binop (op "sub")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "y"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "x"))
								(p-list
									(patterns
										(p-assign (ident "y"))))))))
				(value
					(e-binop (op "mul")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "y")))))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
