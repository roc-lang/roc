# META
~~~ini
description=Match expression demonstrating variable shadowing between outer scope and branches
type=expr
~~~
# SOURCE
~~~roc
match (value, other) {
    (Some(x), y) => x + y
    (None, x) => x * 2
}
~~~
# EXPECTED
UNDEFINED VARIABLE - variable_shadowing.md:1:8:1:13
UNDEFINED VARIABLE - variable_shadowing.md:1:15:1:20
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `value` in this scope.
Is there an `import` or `exposing` missing up-top?

**variable_shadowing.md:1:8:1:13:**
```roc
match (value, other) {
```
       ^^^^^


**UNDEFINED VARIABLE**
Nothing is named `other` in this scope.
Is there an `import` or `exposing` missing up-top?

**variable_shadowing.md:1:15:1:20:**
```roc
match (value, other) {
```
              ^^^^^


# TOKENS
~~~zig
KwMatch,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpenCurly,
OpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
OpenRound,UpperIdent,Comma,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpStar,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-tuple
		(e-ident (raw "value"))
		(e-ident (raw "other")))
	(branches
		(branch
			(p-tuple
				(p-tag (raw "Some")
					(p-ident (raw "x")))
				(p-ident (raw "y")))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-ident (raw "y"))))
		(branch
			(p-tuple
				(p-tag (raw "None"))
				(p-ident (raw "x")))
			(e-binop (op "*")
				(e-ident (raw "x"))
				(e-int (raw "2"))))))
~~~
# FORMATTED
~~~roc
match (value, other) {
	(Some(x), y) => x + y
	(None, x) => x * 2
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-tuple
				(elems
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-runtime-error (tag "ident_not_in_scope")))))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-tuple
							(patterns
								(p-applied-tag)
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
						(p-tuple
							(patterns
								(p-applied-tag)
								(p-assign (ident "x"))))))
				(value
					(e-binop (op "mul")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-num (value "2"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
