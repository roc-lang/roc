# META
~~~ini
description=Match expression with tuple destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match coord {
    (Zero, Zero) => "origin"
    (x, Zero) => x
    (Zero, y) => y
    (x, y) => x
}
~~~
# EXPECTED
UNDEFINED VARIABLE - tuple_patterns.md:1:7:1:12
UNUSED VARIABLE - tuple_patterns.md:5:9:5:10
# PROBLEMS
**UNUSED VARIABLE**
Variable `y` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_y` to suppress this warning.
The unused variable is declared here:
**tuple_patterns.md:5:9:5:10:**
```roc
    (x, y) => x
```
        ^


**TYPE MISMATCH**
The second branch of this `match` does not match the previous branches :
**tuple_patterns.md:3:18:3:19:**
```roc
    (x, Zero) => x
```
                 ^

The second branch is:

    [Zero, ..]

But the previous branches result in:

    Str

All branches in a `match` must have compatible types.
__Note:__ You can wrap branches values in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,OpFatArrow,StringStart,StringPart,StringEnd,
OpenRound,LowerIdent,Comma,UpperIdent,CloseRound,OpFatArrow,LowerIdent,
OpenRound,UpperIdent,Comma,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "coord"))
	(branches
		(branch
			(p-tuple
				(p-tag (raw "Zero"))
				(p-tag (raw "Zero")))
			(e-string
				(e-string-part (raw "origin"))))
		(branch
			(p-tuple
				(p-ident (raw "x"))
				(p-tag (raw "Zero")))
			(e-ident (raw "x")))
		(branch
			(p-tuple
				(p-tag (raw "Zero"))
				(p-ident (raw "y")))
			(e-ident (raw "y")))
		(branch
			(p-tuple
				(p-ident (raw "x"))
				(p-ident (raw "y")))
			(e-ident (raw "x")))))
~~~
# FORMATTED
~~~roc
match coord {
	(Zero, Zero) => "origin"
	(x, Zero) => x
	(Zero, y) => y
	(x, y) => x
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-lookup-local
				(p-assign (ident "coord"))))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-tuple
							(patterns
								(p-applied-tag)
								(p-applied-tag)))))
				(value
					(e-string
						(e-literal (string "origin")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-tuple
							(patterns
								(p-assign (ident "x"))
								(p-applied-tag)))))
				(value
					(e-lookup-local
						(p-assign (ident "x")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-tuple
							(patterns
								(p-applied-tag)
								(p-assign (ident "y"))))))
				(value
					(e-lookup-local
						(p-assign (ident "y")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-tuple
							(patterns
								(p-assign (ident "x"))
								(p-assign (ident "y"))))))
				(value
					(e-lookup-local
						(p-assign (ident "x"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
