# META
~~~ini
description=Structural records should unify with nominal records when type-annotated
type=snippet
~~~
# SOURCE
~~~roc
Person := { name : Str, age : U64 }

alice : Person
alice = { name: "Alice", age: 30 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Person")
				(args))
			(ty-record
				(anno-record-field (name "name")
					(ty (name "Str")))
				(anno-record-field (name "age")
					(ty (name "U64")))))
		(s-type-anno (name "alice")
			(ty (name "Person")))
		(s-decl
			(p-ident (raw "alice"))
			(e-record
				(field (field "name")
					(e-string
						(e-string-part (raw "Alice"))))
				(field (field "age")
					(e-int (raw "30")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "alice"))
		(e-record
			(fields
				(field (name "name")
					(e-string
						(e-literal (string "Alice"))))
				(field (name "age")
					(e-num (value "30")))))
		(annotation
			(ty-lookup (name "Person") (local))))
	(s-nominal-decl
		(ty-header (name "Person"))
		(ty-record
			(field (field "name")
				(ty-lookup (name "Str") (builtin)))
			(field (field "age")
				(ty-lookup (name "U64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Person")))
	(type_decls
		(nominal (type "Person")
			(ty-header (name "Person"))))
	(expressions
		(expr (type "Person"))))
~~~
