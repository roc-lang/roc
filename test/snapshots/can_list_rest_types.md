# META
~~~ini
description=List rest patterns should have correct list types matching element types
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [first, .. as restNums] => restNums
    [] => []
}
~~~
# EXPECTED
UNDEFINED VARIABLE - can_list_rest_types.md:1:7:1:14
UNUSED VARIABLE - can_list_rest_types.md:2:6:2:11
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `numbers` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_list_rest_types.md:1:7:1:14:**
```roc
match numbers {
```
      ^^^^^^^


**UNUSED VARIABLE**
Variable `first` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_first` to suppress this warning.
The unused variable is declared here:
**can_list_rest_types.md:2:6:2:11:**
```roc
    [first, .. as restNums] => restNums
```
     ^^^^^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,CloseSquare,OpFatArrow,OpenSquare,CloseSquare,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "numbers"))
	(branches
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-list-rest (name "restNums")))
			(e-ident (raw "restNums")))
		(branch
			(p-list)
			(e-list))))
~~~
# FORMATTED
~~~roc
match numbers {
	[first, .. as restNums] => restNums
	[] => []
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
								(p-assign (ident "first")))
							(rest-at (index 1)
								(p-assign (ident "restNums"))))))
				(value
					(e-lookup-local
						(p-assign (ident "restNums")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns))))
				(value
					(e-empty_list))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
