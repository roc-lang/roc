# META
~~~ini
description=Test list pattern with rest in middle
type=expr
~~~
# SOURCE
~~~roc
|list| match list {
    [first, .. as middle, last] => first + last
    _ => 0
}
~~~
# EXPECTED
UNUSED VARIABLE - can_list_rest_middle.md:1:1:1:1
# PROBLEMS
**UNUSED VARIABLE**
Variable `middle` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_middle` to suppress this warning.
The unused variable is declared here:
**can_list_rest_middle.md:1:1:1:1:**
```roc
|list| match list {
```
^


# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,Comma,DoubleDot,KwAs,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
Underscore,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "list")))
	(e-match
		(e-ident (raw "list"))
		(branches
			(branch
				(p-list
					(p-ident (raw "first"))
					(p-list-rest (name "middle"))
					(p-ident (raw "last")))
				(e-binop (op "+")
					(e-ident (raw "first"))
					(e-ident (raw "last"))))
			(branch
				(p-underscore)
				(e-int (raw "0"))))))
~~~
# FORMATTED
~~~roc
|list| match list {
	[first, .. as middle, last] => first + last
	_ => 0
}
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "list")))
	(e-match
		(match
			(cond
				(e-lookup-local
					(p-assign (ident "list"))))
			(branches
				(branch
					(patterns
						(pattern (degenerate false)
							(p-list
								(patterns
									(p-assign (ident "first"))
									(p-assign (ident "last")))
								(rest-at (index 1)
									(p-assign (ident "middle"))))))
					(value
						(e-binop (op "add")
							(e-lookup-local
								(p-assign (ident "first")))
							(e-lookup-local
								(p-assign (ident "last"))))))
				(branch
					(patterns
						(pattern (degenerate false)
							(p-underscore)))
					(value
						(e-num (value "0"))))))))
~~~
# TYPES
~~~clojure
(expr (type "List(a) -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, a -> a]"))
~~~
