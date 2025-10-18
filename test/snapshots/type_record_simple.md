# META
~~~ini
description=Simple record type in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

get_name : { name: Str, age: U64 } -> Str
get_name = |person| person.name

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,
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
		(s-type-anno (name "get_name")
			(ty-fn
				(ty-record
					(anno-record-field (name "name")
						(ty (name "Str")))
					(anno-record-field (name "age")
						(ty (name "U64"))))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "get_name"))
			(e-lambda
				(args
					(p-ident (raw "person")))
				(e-field-access
					(e-ident (raw "person"))
					(e-ident (raw "name")))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-record)))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

get_name : { name : Str, age : U64 } -> Str
get_name = |person| person.name

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "get_name"))
		(e-lambda
			(args
				(p-assign (ident "person")))
			(e-dot-access (field "name")
				(receiver
					(e-lookup-local
						(p-assign (ident "person"))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-record
						(field (field "name")
							(ty-lookup (name "Str") (builtin)))
						(field (field "age")
							(ty-lookup (name "U64") (builtin))))
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
		(patt (type "{ age: Num(Int(Unsigned64)), name: Str } -> Str"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "{ age: Num(Int(Unsigned64)), name: Str } -> Str"))
		(expr (type "_arg -> {}"))))
~~~
