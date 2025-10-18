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
UNDEFINED VARIABLE - wrong_arrow.md:1:7:1:8
# PROBLEMS
**PARSE ERROR**
Match branches use `=>` instead of `->`.

**wrong_arrow.md:2:8:2:8:**
```roc
    [] -> Err(EmptyList)
```
       ^


**PARSE ERROR**
Match branches use `=>` instead of `->`.

**wrong_arrow.md:3:13:3:13:**
```roc
    [.., e] -> Ok(e)
```
            ^


**UNDEFINED VARIABLE**
Nothing is named `l` in this scope.
Is there an `import` or `exposing` missing up-top?

**wrong_arrow.md:1:7:1:8:**
```roc
match l {
```
      ^


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
					(e-nominal (nominal "Result")
						(e-tag (name "Err")
							(args
								(e-tag (name "EmptyList")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "e")))
							(rest-at (index 0)))))
				(value
					(e-nominal (nominal "Result")
						(e-tag (name "Ok")
							(args
								(e-lookup-local
									(p-assign (ident "e")))))))))))
~~~
# TYPES
~~~clojure
(expr (type "Result(ok, [EmptyList]_others)"))
~~~
