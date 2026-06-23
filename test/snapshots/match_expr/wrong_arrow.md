# META
~~~ini
description=Match expression with wrong arrow
type=expr
~~~
# SOURCE
~~~roc
match l {
    [] -> Err(EmptyList)
    [.., e] -> Ok(e)
}
~~~
# EXPECTED
PARSE ERROR - wrong_arrow.md:2:8:2:8
PARSE ERROR - wrong_arrow.md:3:13:3:13
# PROBLEMS

┌─────────────┐
│ PARSE ERROR ├─ Match branches use `=>` instead of `->`. ────────────────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  [] -> Err(EmptyList)                                                      │
 │     ‾                                                                      │
 └──────────────────────────────────────────────────────── wrong_arrow.md:2:8 ┘



┌─────────────┐
│ PARSE ERROR ├─ Match branches use `=>` instead of `->`. ────────────────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  [.., e] -> Ok(e)                                                          │
 │          ‾                                                                 │
 └─────────────────────────────────────────────────────── wrong_arrow.md:3:13 ┘


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,CloseSquare,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
OpenSquare,DoubleDot,Comma,LowerIdent,CloseSquare,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "l"))
	(branches
		(branch
			(p-list)
			(e-apply
				(e-tag (raw "Err"))
				(e-tag (raw "EmptyList"))))
		(branch
			(p-list
				(p-list-rest)
				(p-ident (raw "e")))
			(e-apply
				(e-tag (raw "Ok"))
				(e-ident (raw "e"))))))
~~~
# FORMATTED
~~~roc
match l {
	[] => Err(EmptyList)
	[.., e] => Ok(e)
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
							(patterns))))
				(value
					(e-tag (name "Err")
						(args
							(e-tag (name "EmptyList"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "e")))
							(rest-at (index 0)))))
				(value
					(e-tag (name "Ok")
						(args
							(e-lookup-local
								(p-assign (ident "e"))))))))))
~~~
# TYPES
~~~clojure
(expr (type "[Err([EmptyList, ..]), Ok(_a), ..]"))
~~~
