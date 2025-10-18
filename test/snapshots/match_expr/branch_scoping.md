# META
~~~ini
description=Comprehensive test for match branch scoping with variable isolation
type=expr
~~~
# SOURCE
~~~roc
match result {
    Ok(value) => value + 1
    Err(value) => value - 1
    Ok(different) => different * 2
    Err(different) => different / 2
}
~~~
# EXPECTED
UNDEFINED VARIABLE - branch_scoping.md:1:7:1:13
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `result` in this scope.
Is there an `import` or `exposing` missing up-top?

**branch_scoping.md:1:7:1:13:**
```roc
match result {
```
      ^^^^^^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpPlus,Int,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpBinaryMinus,Int,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpStar,Int,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpSlash,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "result"))
	(branches
		(branch
			(p-tag (raw "Ok")
				(p-ident (raw "value")))
			(e-binop (op "+")
				(e-ident (raw "value"))
				(e-int (raw "1"))))
		(branch
			(p-tag (raw "Err")
				(p-ident (raw "value")))
			(e-binop (op "-")
				(e-ident (raw "value"))
				(e-int (raw "1"))))
		(branch
			(p-tag (raw "Ok")
				(p-ident (raw "different")))
			(e-binop (op "*")
				(e-ident (raw "different"))
				(e-int (raw "2"))))
		(branch
			(p-tag (raw "Err")
				(p-ident (raw "different")))
			(e-binop (op "/")
				(e-ident (raw "different"))
				(e-int (raw "2"))))))
~~~
# FORMATTED
~~~roc
match result {
	Ok(value) => value + 1
	Err(value) => value - 1
	Ok(different) => different * 2
	Err(different) => different / 2
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
						(p-applied-tag)))
				(value
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "value")))
						(e-num (value "1")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-binop (op "sub")
						(e-lookup-local
							(p-assign (ident "value")))
						(e-num (value "1")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-binop (op "mul")
						(e-lookup-local
							(p-assign (ident "different")))
						(e-num (value "2")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-binop (op "div")
						(e-lookup-local
							(p-assign (ident "different")))
						(e-num (value "2"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
