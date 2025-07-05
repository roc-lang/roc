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
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `len` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `len` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:13),OpenCurly(1:14-1:15),Newline(1:1-1:1),
OpenCurly(2:5-2:6),LowerIdent(2:7-2:17),Comma(2:17-2:18),DoubleDot(2:19-2:21),LowerIdent(2:21-2:27),CloseCurly(2:28-2:29),OpFatArrow(2:30-2:32),UpperIdent(2:33-2:36),NoSpaceDotLowerIdent(2:36-2:40),NoSpaceOpenRound(2:40-2:41),LowerIdent(2:41-2:51),CloseRound(2:51-2:52),OpGreaterThan(2:53-2:54),UpperIdent(2:55-2:58),NoSpaceDotLowerIdent(2:58-2:62),NoSpaceOpenRound(2:62-2:63),LowerIdent(2:63-2:69),NoSpaceDotLowerIdent(2:69-2:79),CloseRound(2:79-2:80),Newline(1:1-1:1),
CloseCurly(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.13 (raw "person"))
	(branches
		(branch @2.5-3.2
			(p-record @2.5-2.29
				(field @2.7-2.18 (name "first_name") (rest false))
				(field @2.19-2.29 (name "others") (rest true)))
			(e-binop @2.33-3.2 (op ">")
				(e-apply @2.33-2.52
					(e-ident @2.33-2.40 (raw "Str.len"))
					(e-ident @2.41-2.51 (raw "first_name")))
				(e-apply @2.55-2.80
					(e-ident @2.55-2.62 (raw "Str.len"))
					(e-field-access @2.63-2.80
						(e-ident @2.63-2.69 (raw "others"))
						(e-ident @2.69-2.79 (raw "last_name"))))))))
~~~
# FORMATTED
~~~roc
match person {
	{ first_name, ..others } => Str.len(first_name) > Str.len(others.last_name)
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-3.2
	(match @1.1-3.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(p-record-destructure @2.5-2.29 (degenerate false)
						(destructs
							(record-destruct @2.7-2.18 (label "first_name") (ident "first_name")
								(required))
							(record-destruct @2.19-2.29 (label "others") (ident "others")
								(required)))))
				(value
					(e-binop @2.33-3.2 (op "gt")
						(e-call @2.33-2.52
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-lookup-local @2.41-2.51
								(pattern @2.7-2.18)))
						(e-call @2.55-2.80
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-dot-access @2.63-2.80 (field "last_name")
								(receiver
									(e-lookup-local @2.63-2.69
										(pattern @2.19-2.29)))))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-3.2 (type "*"))
~~~
