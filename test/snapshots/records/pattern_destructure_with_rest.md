# META
~~~ini
description=Record destructuring with rest pattern
type=expr
~~~
# SOURCE
~~~roc
match person {
    { first_name, ..others } => Str.len(first_name) > Str.len(others.last_name)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,Comma,DoubleDot,LowerIdent,CloseCurly,OpFatArrow,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpGreaterThan,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "person"))
	(branches
		(branch
			(p-record
				(field (name "first_name") (rest false))
				(field (name "others") (rest true)))
			(e-binop (op ">")
				(e-apply
					(e-ident (raw "Str.len"))
					(e-ident (raw "first_name")))
				(e-apply
					(e-ident (raw "Str.len"))
					(e-field-access
						(e-ident (raw "others"))
						(e-ident (raw "last_name"))))))))
~~~
# FORMATTED
~~~roc
match person {
	{ first_name, ..others } => Str.len(first_name) > Str.len(others.last_name)
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
						(p-record-destructure
							(destructs
								(record-destruct (label "first_name") (ident "first_name")
									(required
										(p-assign (ident "first_name"))))
								(record-destruct (label "others") (ident "others")
									(rest-pattern
										(p-assign (ident "others"))))))))
				(value
					(e-binop (op "gt")
						(e-call
							(e-runtime-error (tag "nested_value_not_found"))
							(e-lookup-local
								(p-assign (ident "first_name"))))
						(e-call
							(e-runtime-error (tag "nested_value_not_found"))
							(e-field-access (field "last_name")
								(receiver
									(e-lookup-local
										(p-assign (ident "others"))))))))))))
~~~
# TYPES
~~~clojure
(expr (type "Bool"))
~~~
