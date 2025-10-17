# META
~~~ini
description=Match expression with mixed literal and variable patterns in lists
type=expr
~~~
# SOURCE
~~~roc
match sequence {
    [0, count] => count
    [1, x, 3] => x
    [42, value] => value
    [first, 99] => first
    [] => 0
}
~~~
# EXPECTED
UNDEFINED VARIABLE - list_mixed_literals.md:1:7:1:15
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `sequence` in this scope.
Is there an `import` or `exposing` missing up-top?

**list_mixed_literals.md:1:7:1:15:**
```roc
match sequence {
```
      ^^^^^^^^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,Int,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,Int,Comma,LowerIdent,Comma,Int,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,Int,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,LowerIdent,Comma,Int,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,CloseSquare,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "sequence"))
	(branches
		(branch
			(p-list
				(p-int (raw "0"))
				(p-ident (raw "count")))
			(e-ident (raw "count")))
		(branch
			(p-list
				(p-int (raw "1"))
				(p-ident (raw "x"))
				(p-int (raw "3")))
			(e-ident (raw "x")))
		(branch
			(p-list
				(p-int (raw "42"))
				(p-ident (raw "value")))
			(e-ident (raw "value")))
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-int (raw "99")))
			(e-ident (raw "first")))
		(branch
			(p-list)
			(e-int (raw "0")))))
~~~
# FORMATTED
~~~roc
match sequence {
	[0, count] => count
	[1, x, 3] => x
	[42, value] => value
	[first, 99] => first
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
								(p-num (value "0"))
								(p-assign (ident "count"))))))
				(value
					(e-lookup-local
						(p-assign (ident "count")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-num (value "1"))
								(p-assign (ident "x"))
								(p-num (value "3"))))))
				(value
					(e-lookup-local
						(p-assign (ident "x")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-num (value "42"))
								(p-assign (ident "value"))))))
				(value
					(e-lookup-local
						(p-assign (ident "value")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "first"))
								(p-num (value "99"))))))
				(value
					(e-lookup-local
						(p-assign (ident "first")))))
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
