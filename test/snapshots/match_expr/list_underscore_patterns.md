# META
~~~ini
description=Match expression with underscore patterns in list matching
type=expr
~~~
# SOURCE
~~~roc
match items {
    [_] => 1 # pattern match on a list with a single (ignored) element
    [.., last] => last # pattern match on the last item in the list
    [first, ..] => first # pattern match on the first item in the list
    [_, _, third] => third # pattern match on the third item in the list
    [x, _, _, y] => x + y # first + fourth item in the list
    [] => 0 # match an empty list
}
~~~
# EXPECTED
UNDEFINED VARIABLE - list_underscore_patterns.md:1:7:1:12
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `items` in this scope.
Is there an `import` or `exposing` missing up-top?

**list_underscore_patterns.md:1:7:1:12:**
```roc
match items {
```
      ^^^^^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,Underscore,CloseSquare,OpFatArrow,Int,
OpenSquare,DoubleDot,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,LowerIdent,Comma,DoubleDot,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,Underscore,Comma,Underscore,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,LowerIdent,Comma,Underscore,Comma,Underscore,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
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
				(p-underscore))
			(e-int (raw "1")))
		(branch
			(p-list
				(p-list-rest)
				(p-ident (raw "last")))
			(e-ident (raw "last")))
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-list-rest))
			(e-ident (raw "first")))
		(branch
			(p-list
				(p-underscore)
				(p-underscore)
				(p-ident (raw "third")))
			(e-ident (raw "third")))
		(branch
			(p-list
				(p-ident (raw "x"))
				(p-underscore)
				(p-underscore)
				(p-ident (raw "y")))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-ident (raw "y"))))
		(branch
			(p-list)
			(e-int (raw "0")))))
~~~
# FORMATTED
~~~roc
match items {
	[_] => 1 # pattern match on a list with a single (ignored) element
	[.., last] => last # pattern match on the last item in the list
	[first, ..] => first # pattern match on the first item in the list
	[_, _, third] => third # pattern match on the third item in the list
	[x, _, _, y] => x + y # first + fourth item in the list
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
								(p-underscore)))))
				(value
					(e-num (value "1"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "last")))
							(rest-at (index 0)))))
				(value
					(e-lookup-local
						(p-assign (ident "last")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "first")))
							(rest-at (index 1)))))
				(value
					(e-lookup-local
						(p-assign (ident "first")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-underscore)
								(p-underscore)
								(p-assign (ident "third"))))))
				(value
					(e-lookup-local
						(p-assign (ident "third")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "x"))
								(p-underscore)
								(p-underscore)
								(p-assign (ident "y"))))))
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
							(patterns))))
				(value
					(e-num (value "0")))))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
