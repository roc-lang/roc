# META
~~~ini
description=Unnamed padding fields are allowed in a nominal record declaration
type=snippet
~~~
# SOURCE
~~~roc
Padded := { a : U8, _ : U8, b : U32 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,Underscore,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Padded")
				(args))
			(ty-record
				(anno-record-field (name "a")
					(ty (name "U8")))
				(anno-record-field (name "_")
					(ty (name "U8")))
				(anno-record-field (name "b")
					(ty (name "U32")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "Padded"))
		(ty-record
			(field (field "a")
				(ty-lookup (name "U8") (builtin)))
			(field (field "malformed_field")
				(ty-lookup (name "U8") (builtin)))
			(field (field "b")
				(ty-lookup (name "U32") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "Padded")
			(ty-header (name "Padded"))))
	(expressions))
~~~
