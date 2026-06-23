# META
~~~ini
description=An UNANNOTATED non-expansive value is not generalized (annotation is the opt-in), so using it at two concrete types is a type mismatch (tier-2 gate)
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

bare = []

nums : List(U64)
nums = bare

strs : List(Str)
strs = bare

main! = |_| {}
~~~
# EXPECTED
TYPE MISMATCH - generalize_annotated_value_unannotated_not_generalized.md:9:8:9:12
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ This expression is used in an unexpected way. ─────────────┐
└┬──────────────┘                                                             │
 │                                                                            │
 │  strs = bare                                                               │
 │         ‾‾‾‾                                                               │
 └───────────── generalize_annotated_value_unannotated_not_generalized.md:9:8 ┘

    It has the type:

        List(U64)

    But the annotation says it should be:

        List(Str)

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,OpenSquare,CloseSquare,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,CloseCurly,
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
			(p-ident (raw "bare"))
			(e-list))
		(s-type-anno (name "nums")
			(ty-apply
				(ty (name "List"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "nums"))
			(e-ident (raw "bare")))
		(s-type-anno (name "strs")
			(ty-apply
				(ty (name "List"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "strs"))
			(e-ident (raw "bare")))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-record)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "bare"))
		(e-empty_list))
	(d-let
		(p-assign (ident "nums"))
		(e-runtime-error (tag "erroneous_value_use"))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "strs"))
		(e-runtime-error (tag "erroneous_value_use"))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "List(Str)"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "List(Str)"))
		(expr (type "_arg -> {}"))))
~~~
