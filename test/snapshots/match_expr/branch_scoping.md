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
REDUNDANT PATTERN - branch_scoping.md:1:1:6:2
REDUNDANT PATTERN - branch_scoping.md:1:1:6:2
# PROBLEMS
**REDUNDANT PATTERN**
The third branch of this `match` is redundant:
**branch_scoping.md:1:1:6:2:**
```roc
match result {
    Ok(value) => value + 1
    Err(value) => value - 1
    Ok(different) => different * 2
    Err(different) => different / 2
}
```

This pattern can never match because earlier patterns already cover all the values it would match.

**REDUNDANT PATTERN**
The fourth branch of this `match` is redundant:
**branch_scoping.md:1:1:6:2:**
```roc
match result {
    Ok(value) => value + 1
    Err(value) => value - 1
    Ok(different) => different * 2
    Err(different) => different / 2
}
```

This pattern can never match because earlier patterns already cover all the values it would match.

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
			(e-lookup-local
				(p-assign (ident "result"))))
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
(expr (type "a where [a.div_by : a, Dec -> a, a.minus : a, Dec -> a, a.plus : a, Dec -> a, a.times : a, Dec -> a]"))
~~~
