# META
~~~ini
description=Match expression with mixed tag and list patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match data {
    Ok([x, y]) => x + y
    Err(x) => x - 1
    Ok([x]) => x * 2
    Err(y) => y / 2
}
~~~
# EXPECTED
NON-EXHAUSTIVE MATCH - mixed_pattern_scoping.md:1:1:6:2
REDUNDANT PATTERN - mixed_pattern_scoping.md:1:1:6:2
# PROBLEMS
**NON-EXHAUSTIVE MATCH**
This `match` expression doesn't cover all possible cases:
**mixed_pattern_scoping.md:1:1:6:2:**
```roc
match data {
    Ok([x, y]) => x + y
    Err(x) => x - 1
    Ok([x]) => x * 2
    Err(y) => y / 2
}
```

The value being matched on has type:
        _[Err(a), Ok(List(a))]
  where [
    a.div_by : a, b -> a,
    a.minus : a, c -> a,
    a.plus : a, a -> a,
    a.times : a, d -> a,
    b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]),
    c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]),
    d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]),
  ]_

Missing patterns:
        Ok []

Hint: Add branches to handle these cases, or use `_` to match anything.

**REDUNDANT PATTERN**
The fourth branch of this `match` is redundant:
**mixed_pattern_scoping.md:1:1:6:2:**
```roc
match data {
    Ok([x, y]) => x + y
    Err(x) => x - 1
    Ok([x]) => x * 2
    Err(y) => y / 2
}
```

This pattern can never match because earlier patterns already cover all the values it would match.

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,CloseRound,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpBinaryMinus,Int,
UpperIdent,NoSpaceOpenRound,OpenSquare,LowerIdent,CloseSquare,CloseRound,OpFatArrow,LowerIdent,OpStar,Int,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpSlash,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "data"))
	(branches
		(branch
			(p-tag (raw "Ok")
				(p-list
					(p-ident (raw "x"))
					(p-ident (raw "y"))))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-ident (raw "y"))))
		(branch
			(p-tag (raw "Err")
				(p-ident (raw "x")))
			(e-binop (op "-")
				(e-ident (raw "x"))
				(e-int (raw "1"))))
		(branch
			(p-tag (raw "Ok")
				(p-list
					(p-ident (raw "x"))))
			(e-binop (op "*")
				(e-ident (raw "x"))
				(e-int (raw "2"))))
		(branch
			(p-tag (raw "Err")
				(p-ident (raw "y")))
			(e-binop (op "/")
				(e-ident (raw "y"))
				(e-int (raw "2"))))))
~~~
# FORMATTED
~~~roc
match data {
	Ok([x, y]) => x + y
	Err(x) => x - 1
	Ok([x]) => x * 2
	Err(y) => y / 2
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-lookup-local
				(p-assign (ident "data"))))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "y"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-binop (op "sub")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-num (value "1")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-binop (op "mul")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-num (value "2")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-binop (op "div")
						(e-lookup-local
							(p-assign (ident "y")))
						(e-num (value "2"))))))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.div_by : a, Dec -> a, a.minus : a, Dec -> a, a.plus : a, a -> a, a.times : a, Dec -> a]"))
~~~
