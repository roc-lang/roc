# META
~~~ini
description=A nominal type satisfies a where alias by providing its methods
type=snippet
~~~
# SOURCE
~~~roc
a.Stringable : where [a.to_str : a -> Str]

Name := [Name(Str)].{
	to_str : Name -> Str
	to_str = |Name.Name(str)| str
}

stringify : a -> Str where [a.Stringable]
stringify = |value| value.to_str()

main = stringify(Name.Name("hi"))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,NoSpaceDotUpperIdent,OpColon,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,LowerIdent,
CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotUpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name ".Stringable")
				(args))
			(ty-var (raw "a"))
			(where
				(method (mod-of "a") (name "to_str")
					(args
						(ty-var (raw "a")))
					(ty (name "Str")))))
		(s-type-decl
			(header (name "Name")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Name"))
						(ty (name "Str")))))
			(associated
				(s-type-anno (name "to_str")
					(ty-fn
						(ty (name "Name"))
						(ty (name "Str"))))
				(s-decl
					(p-ident (raw "to_str"))
					(e-lambda
						(args
							(p-tag (raw ".Name")
								(p-ident (raw "str"))))
						(e-ident (raw "str"))))))
		(s-type-anno (name "stringify")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "Str")))
			(where
				(alias (mod-of "a")
					(ty (name "Stringable")))))
		(s-decl
			(p-ident (raw "stringify"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-method-call (method ".to_str")
					(receiver
						(e-ident (raw "value")))
					(args))))
		(s-decl
			(p-ident (raw "main"))
			(e-apply
				(e-ident (raw "stringify"))
				(e-apply
					(e-tag (raw "Name.Name"))
					(e-string
						(e-string-part (raw "hi"))))))))
~~~
# FORMATTED
~~~roc
a.Stringable :  where [a.to_str : a -> Str]

Name := [Name(Str)].{
	to_str : Name -> Str
	to_str = |Name.Name(str)| str
}

stringify : a -> Str where [a.Stringable]
stringify = |value| value.to_str()

main = stringify(Name.Name("hi"))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "where_alias_satisfied_by_nominal.Name.to_str"))
		(e-lambda
			(args
				(p-nominal
					(p-applied-tag)))
			(e-lookup-local
				(p-assign (ident "str"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Name") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "stringify"))
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-dispatch-call (method "to_str") (constraint-fn-var 282)
				(receiver
					(e-lookup-local
						(p-assign (ident "value"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "Str") (builtin)))
			(where
				(alias
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-lookup (name "Stringable") (local))))))
	(d-let
		(p-assign (ident "main"))
		(e-call (constraint-fn-var 301)
			(e-lookup-local
				(p-assign (ident "stringify")))
			(e-nominal (nominal "Name")
				(e-tag (name "Name")
					(args
						(e-string
							(e-literal (string "hi"))))))))
	(s-where-alias-decl
		(ty-header (name "Stringable"))
		(ty-rigid-var (name "a"))
		(where
			(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_str")
				(args
					(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
				(ty-lookup (name "Str") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Name"))
		(ty-tag-union
			(ty-tag-name (name "Name")
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Name -> Str"))
		(patt (type "a -> Str where [a.to_str : a -> Str]"))
		(patt (type "Str")))
	(type_decls
		(where-alias (type "a where [a.to_str : a -> Str]")
			(ty-header (name "Stringable")))
		(nominal (type "Name")
			(ty-header (name "Name"))))
	(expressions
		(expr (type "Name -> Str"))
		(expr (type "a -> Str where [a.to_str : a -> Str]"))
		(expr (type "Str"))))
~~~
