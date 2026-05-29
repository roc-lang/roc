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


**NON-EXHAUSTIVE MATCH**
This `match` expression doesn't cover all possible cases:
**list_patterns_err_multiple_rest.md:1:1:3:2:**
```roc
match numbers {
    [.., middle, ..] => ... # error, multiple rest patterns not allowed
}
```

The value being matched on has type:
        _List(_a)_

Missing patterns:
        []

Hint: Add branches to handle these cases, or use `_` to match anything.

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
			(e-lookup-local
				(p-assign (ident "numbers"))))
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
