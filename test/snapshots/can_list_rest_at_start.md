# META
~~~ini
description=Test list pattern with rest at start
type=expr
~~~
# SOURCE
~~~roc
|list| match list {
    [.. as init, last] => last
    [] => 0
}
~~~
# EXPECTED
UNUSED VARIABLE - can_list_rest_at_start.md:1:1:1:1
# PROBLEMS
**UNUSED VARIABLE**
Variable `init` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_init` to suppress this warning.
The unused variable is declared here:
**can_list_rest_at_start.md:1:1:1:1:**
```roc
|list| match list {
```
^


# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
OpenSquare,DoubleDot,KwAs,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,CloseSquare,OpFatArrow,Int,
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
					(p-list-rest (name "init"))
					(p-ident (raw "last")))
				(e-ident (raw "last")))
			(branch
				(p-list)
				(e-int (raw "0"))))))
~~~
# FORMATTED
~~~roc
|list| match list {
	[.. as init, last] => last
	[] => 0
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
									(p-assign (ident "last")))
								(rest-at (index 0)
									(p-assign (ident "init"))))))
					(value
						(e-lookup-local
							(p-assign (ident "last")))))
				(branch
					(patterns
						(pattern (degenerate false)
							(p-list
								(patterns))))
					(value
						(e-num (value "0"))))))))
~~~
# TYPES
~~~clojure
(expr (type "List(a) -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
