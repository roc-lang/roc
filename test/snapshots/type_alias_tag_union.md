# META
~~~ini
description=Type alias with tag union and type parameters
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Type alias with type parameters that expands to a tag union
MyResult(ok, err) : [Good(ok), Bad(err)]

# Using the type alias
process : MyResult(Str, I32) -> Str
process = |_result| "processed"

# Another type alias with a single parameter
Option(a) : [Some(a), None]

# Using it with different types
getString : Option(Str) -> Str
getString = |_opt| "default"

getNumber : Option(I32) -> I32
getNumber = |_opt| 0

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,StringStart,StringPart,StringEnd,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,Int,
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
		(s-type-decl
			(header (name "MyResult")
				(args
					(ty-var (raw "ok"))
					(ty-var (raw "err"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Good"))
						(ty-var (raw "ok")))
					(ty-apply
						(ty (name "Bad"))
						(ty-var (raw "err"))))))
		(s-type-anno (name "process")
			(ty-fn
				(ty-apply
					(ty (name "MyResult"))
					(ty (name "Str"))
					(ty (name "I32")))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "process"))
			(e-lambda
				(args
					(p-ident (raw "_result")))
				(e-string
					(e-string-part (raw "processed")))))
		(s-type-decl
			(header (name "Option")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Some"))
						(ty-var (raw "a")))
					(ty (name "None")))))
		(s-type-anno (name "getString")
			(ty-fn
				(ty-apply
					(ty (name "Option"))
					(ty (name "Str")))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "getString"))
			(e-lambda
				(args
					(p-ident (raw "_opt")))
				(e-string
					(e-string-part (raw "default")))))
		(s-type-anno (name "getNumber")
			(ty-fn
				(ty-apply
					(ty (name "Option"))
					(ty (name "I32")))
				(ty (name "I32"))))
		(s-decl
			(p-ident (raw "getNumber"))
			(e-lambda
				(args
					(p-ident (raw "_opt")))
				(e-int (raw "0"))))
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
		(p-assign (ident "process"))
		(e-lambda
			(args
				(p-assign (ident "_result")))
			(e-string
				(e-literal (string "processed"))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "MyResult") (local)
					(ty-lookup (name "Str") (external-module "Str"))
					(ty-lookup (name "I32") (builtin)))
				(ty-lookup (name "Str") (external-module "Str")))))
	(d-let
		(p-assign (ident "getString"))
		(e-lambda
			(args
				(p-assign (ident "_opt")))
			(e-string
				(e-literal (string "default"))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Option") (local)
					(ty-lookup (name "Str") (external-module "Str")))
				(ty-lookup (name "Str") (external-module "Str")))))
	(d-let
		(p-assign (ident "getNumber"))
		(e-lambda
			(args
				(p-assign (ident "_opt")))
			(e-num (value "0")))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Option") (local)
					(ty-lookup (name "I32") (builtin)))
				(ty-lookup (name "I32") (builtin)))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-empty_record)))
	(s-alias-decl
		(ty-header (name "MyResult")
			(ty-args
				(ty-rigid-var (name "ok"))
				(ty-rigid-var (name "err"))))
		(ty-tag-union
			(ty-tag-name (name "Good")
				(ty-rigid-var-lookup (ty-rigid-var (name "ok"))))
			(ty-tag-name (name "Bad")
				(ty-rigid-var-lookup (ty-rigid-var (name "err"))))))
	(s-alias-decl
		(ty-header (name "Option")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Some")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(ty-tag-name (name "None")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "MyResult(Str, Num(Int(Signed32))) -> Str"))
		(patt (type "Option(Str) -> Str"))
		(patt (type "Option(Num(Int(Signed32))) -> Num(Int(Signed32))"))
		(patt (type "_arg -> {}")))
	(type_decls
		(alias (type "MyResult(ok, err)")
			(ty-header (name "MyResult")
				(ty-args
					(ty-rigid-var (name "ok"))
					(ty-rigid-var (name "err")))))
		(alias (type "Option(a)")
			(ty-header (name "Option")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "MyResult(Str, Num(Int(Signed32))) -> Str"))
		(expr (type "Option(Str) -> Str"))
		(expr (type "Option(Num(Int(Signed32))) -> Num(Int(Signed32))"))
		(expr (type "_arg -> {}"))))
~~~
