# META
~~~ini
description=A top-level annotated value carrying a static-dispatch where-constraint is rejected as a polymorphic value, since top-level values can't have free type variables (tier-2)
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

items : List(a) where [a.to_str : a -> Str]
items = []

main! = |_| {}
~~~
# EXPECTED
POLYMORPHIC VALUE - generalize_annotated_value_constrained.md:4:1:4:6
# PROBLEMS

┌───────────────────┐
│ POLYMORPHIC VALUE ├─ This top-level value still has an unresolved ──────────┐
└┬──────────────────┘  polymorphic type.                                      │
 │                                                                            │
 │  items = []                                                                │
 │  ‾‾‾‾‾                                                                     │
 └───────────────────────────── generalize_annotated_value_constrained.md:4:1 ┘

    Its type is:
    List(a) where [a.to_str : a -> Str]
    Add an annotation or use this value in a way that fixes its concrete type.

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpenSquare,CloseSquare,
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
		(s-type-anno (name "items")
			(ty-apply
				(ty (name "List"))
				(ty-var (raw "a")))
			(where
				(method (module-of "a") (name "to_str")
					(args
						(ty-var (raw "a")))
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "items"))
			(e-list))
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
		(p-assign (ident "items"))
		(e-empty_list)
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-rigid-var (name "a")))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_str")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-lookup (name "Str") (builtin))))))
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
		(patt (type "List(a) where [a.to_str : a -> Str]"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "List(a) where [a.to_str : a -> Str]"))
		(expr (type "_arg -> {}"))))
~~~
