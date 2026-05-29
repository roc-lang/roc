# META
~~~ini
description=Match expression with nested patterns (tags containing records, lists with tags)
type=expr
~~~
# SOURCE
~~~roc
match data {
    Container({ items: [First(x), .. as rest] }) => x + List.len(rest)
    Container({ items: [] }) => 0
    Wrapper([Tag(value), Other(y)]) => value + y
    Simple(x) => x
}
~~~
# EXPECTED
NON-EXHAUSTIVE MATCH - nested_patterns.md:1:1:6:2
# PROBLEMS
**NON-EXHAUSTIVE MATCH**
This `match` expression doesn't cover all possible cases:
**nested_patterns.md:1:1:6:2:**
```roc
match data {
    Container({ items: [First(x), .. as rest] }) => x + List.len(rest)
    Container({ items: [] }) => 0
    Wrapper([Tag(value), Other(y)]) => value + y
    Simple(x) => x
}
```

The value being matched on has type:
        _[Container({ items: List([First(a)]), .. }), Simple(a), Wrapper(List([Other(U64), Tag(a), ..]))]
  where [
    a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),
    a.plus : a, U64 -> a,
  ]_

Missing patterns:
        Wrapper []

Hint: Add branches to handle these cases, or use `_` to match anything.

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,CloseCurly,CloseRound,OpFatArrow,LowerIdent,OpPlus,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,OpenSquare,CloseSquare,CloseCurly,CloseRound,OpFatArrow,Int,
UpperIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,CloseRound,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "data"))
	(branches
		(branch
			(p-tag (raw "Container")
				(p-record
					(field (name "items") (rest false)
						(p-list
							(p-tag (raw "First")
								(p-ident (raw "x")))
							(p-list-rest (name "rest"))))))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-apply
					(e-ident (raw "List.len"))
					(e-ident (raw "rest")))))
		(branch
			(p-tag (raw "Container")
				(p-record
					(field (name "items") (rest false)
						(p-list))))
			(e-int (raw "0")))
		(branch
			(p-tag (raw "Wrapper")
				(p-list
					(p-tag (raw "Tag")
						(p-ident (raw "value")))
					(p-tag (raw "Other")
						(p-ident (raw "y")))))
			(e-binop (op "+")
				(e-ident (raw "value"))
				(e-ident (raw "y"))))
		(branch
			(p-tag (raw "Simple")
				(p-ident (raw "x")))
			(e-ident (raw "x")))))
~~~
# FORMATTED
~~~roc
match data {
	Container({ items: [First(x), .. as rest] }) => x + List.len(rest)
	Container({ items: [] }) => 0
	Wrapper([Tag(value), Other(y)]) => value + y
	Simple(x) => x
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
						(e-call
							(e-lookup-external
								(builtin))
							(e-lookup-local
								(p-assign (ident "rest")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-num (value "0"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "value")))
						(e-lookup-local
							(p-assign (ident "y"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-lookup-local
						(p-assign (ident "x"))))))))
~~~
# TYPES
~~~clojure
(expr (type "U64"))
~~~
