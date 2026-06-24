# META
~~~ini
description=An annotated, non-expansive value bound inside a block (not top-level) is still a true scheme, instantiable at two different concrete types (tier-2 generalization)
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

main! = |_| {
    empty : List(a)
    empty = []

    nums : List(U64)
    nums = empty

    strs : List(Str)
    strs = empty

    _ = nums
    _ = strs
    {}
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpenSquare,CloseSquare,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,
Underscore,OpAssign,LowerIdent,
Underscore,OpAssign,LowerIdent,
OpenCurly,CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-type-anno (name "empty")
							(ty-apply
								(ty (name "List"))
								(ty-var (raw "a"))))
						(s-decl
							(p-ident (raw "empty"))
							(e-list))
						(s-type-anno (name "nums")
							(ty-apply
								(ty (name "List"))
								(ty (name "U64"))))
						(s-decl
							(p-ident (raw "nums"))
							(e-ident (raw "empty")))
						(s-type-anno (name "strs")
							(ty-apply
								(ty (name "List"))
								(ty (name "Str"))))
						(s-decl
							(p-ident (raw "strs"))
							(e-ident (raw "empty")))
						(s-decl
							(p-underscore)
							(e-ident (raw "nums")))
						(s-decl
							(p-underscore)
							(e-ident (raw "strs")))
						(e-record)))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

main! = |_| {
	empty : List(a)
	empty = []

	nums : List(U64)
	nums = empty

	strs : List(Str)
	strs = empty

	_ = nums
	_ = strs
	{}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-assign (ident "empty"))
					(e-empty_list))
				(s-let
					(p-assign (ident "nums"))
					(e-lookup-local
						(p-assign (ident "empty"))))
				(s-let
					(p-assign (ident "strs"))
					(e-lookup-local
						(p-assign (ident "empty"))))
				(s-let
					(p-underscore)
					(e-lookup-local
						(p-assign (ident "nums"))))
				(s-let
					(p-underscore)
					(e-lookup-local
						(p-assign (ident "strs"))))
				(e-empty_record)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "_arg -> {}"))))
~~~
