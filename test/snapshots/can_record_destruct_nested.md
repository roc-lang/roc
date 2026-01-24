# META
~~~ini
description=Test nested record destructuring pattern
type=expr
~~~
# SOURCE
~~~roc
|data| match data {
    { simple, nested: { deep } } => simple + deep
    _ => 0
}
~~~
# EXPECTED
REDUNDANT PATTERN - can_record_destruct_nested.md:1:8:4:2
# PROBLEMS
**REDUNDANT PATTERN**
The second branch of this `match` is redundant:
**can_record_destruct_nested.md:1:8:4:2:**
```roc
|data| match data {
    { simple, nested: { deep } } => simple + deep
    _ => 0
}
```

This pattern can never match because earlier patterns already cover all the values it would match.

# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,Comma,LowerIdent,OpColon,OpenCurly,LowerIdent,CloseCurly,CloseCurly,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
Underscore,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "data")))
	(e-match
		(e-ident (raw "data"))
		(branches
			(branch
				(p-record
					(field (name "simple") (rest false))
					(field (name "nested") (rest false)
						(p-record
							(field (name "deep") (rest false)))))
				(e-binop (op "+")
					(e-ident (raw "simple"))
					(e-ident (raw "deep"))))
			(branch
				(p-underscore)
				(e-int (raw "0"))))))
~~~
# FORMATTED
~~~roc
|data| match data {
	{ simple, nested: { deep } } => simple + deep
	_ => 0
}
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "data")))
	(e-match
		(match
			(cond
				(e-lookup-local
					(p-assign (ident "data"))))
			(branches
				(branch
					(patterns
						(pattern (degenerate false)
							(p-record-destructure
								(destructs
									(record-destruct (label "simple") (ident "simple")
										(required
											(p-assign (ident "simple"))))
									(record-destruct (label "nested") (ident "nested")
										(sub-pattern
											(p-record-destructure
												(destructs
													(record-destruct (label "deep") (ident "deep")
														(required
															(p-assign (ident "deep"))))))))))))
					(value
						(e-binop (op "add")
							(e-lookup-local
								(p-assign (ident "simple")))
							(e-lookup-local
								(p-assign (ident "deep"))))))
				(branch
					(patterns
						(pattern (degenerate false)
							(p-underscore)))
					(value
						(e-num (value "0"))))))))
~~~
# TYPES
~~~clojure
(expr (type "{ nested: { deep: a }, simple: b } -> b where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.plus : b, a -> b]"))
~~~
