# META
~~~ini
description=Match expression with more than one rest pattern not permitted, should error
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [.., middle, ..] => ... # error, multiple rest patterns not allowed
}
~~~
# EXPECTED
UNDEFINED VARIABLE - list_patterns_err_multiple_rest.md:1:7:1:14
INVALID PATTERN - :0:0:0:0
UNUSED VARIABLE - list_patterns_err_multiple_rest.md:2:10:2:16
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `numbers` in this scope.
Is there an `import` or `exposing` missing up-top?

**list_patterns_err_multiple_rest.md:1:7:1:14:**
```roc
match numbers {
```
      ^^^^^^^


**INVALID PATTERN**
This pattern contains invalid syntax or uses unsupported features.

**UNUSED VARIABLE**
Variable `middle` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_middle` to suppress this warning.
The unused variable is declared here:
**list_patterns_err_multiple_rest.md:2:10:2:16:**
```roc
    [.., middle, ..] => ... # error, multiple rest patterns not allowed
```
         ^^^^^^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,DoubleDot,Comma,LowerIdent,Comma,DoubleDot,CloseSquare,OpFatArrow,TripleDot,
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
				(p-list-rest)
				(p-ident (raw "middle"))
				(p-list-rest))
			(e-ellipsis))))
~~~
# FORMATTED
~~~roc
match numbers {
	[.., middle, ..] => ...
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
								(p-assign (ident "middle")))
							(rest-at (index 0)))))
				(value
					(e-not-implemented))))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
