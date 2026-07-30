# META
~~~ini
description=Record update sets an optional field (missing to present), overwrites a present one, and copies an unmentioned optional + defaulted field
type=snippet
~~~
# SOURCE
~~~roc
Rec : { req : U8, opt_set ?: U8, opt_copy ?: U8, def : U8 ?? 5 }

base : Rec
base = { req: 1 }

updated = { ..base, req: 9, opt_set: 7 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpQuestion,OpColon,UpperIdent,Comma,LowerIdent,OpQuestion,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpAssign,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Rec")
				(args))
			(ty-record
				(anno-record-field (name "req")
					(ty (name "U8")))
				(anno-record-field (name "opt_set") (optional true)
					(ty (name "U8")))
				(anno-record-field (name "opt_copy") (optional true)
					(ty (name "U8")))
				(anno-record-field (name "def")
					(ty (name "U8"))
					(default
						(e-int (raw "5"))))))
		(s-type-anno (name "base")
			(ty (name "Rec")))
		(s-decl
			(p-ident (raw "base"))
			(e-record
				(field (field "req")
					(e-int (raw "1")))))
		(s-decl
			(p-ident (raw "updated"))
			(e-record
				(ext
					(e-ident (raw "base")))
				(field (field "req")
					(e-int (raw "9")))
				(field (field "opt_set")
					(e-int (raw "7")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "base"))
		(e-record
			(fields
				(field (name "req")
					(e-num (value "1")))))
		(annotation
			(ty-lookup (name "Rec") (local))))
	(d-let
		(p-assign (ident "updated"))
		(e-record
			(ext
				(e-lookup-local
					(p-assign (ident "base"))))
			(fields
				(field (name "req")
					(e-num (value "9")))
				(field (name "opt_set")
					(e-num (value "7"))))))
	(s-alias-decl
		(ty-header (name "Rec"))
		(ty-record
			(field (field "req")
				(ty-lookup (name "U8") (builtin)))
			(field (field "opt_set") (optional true)
				(ty-lookup (name "U8") (builtin)))
			(field (field "opt_copy") (optional true)
				(ty-lookup (name "U8") (builtin)))
			(field (field "def") (defaulted true)
				(ty-lookup (name "U8") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Rec"))
		(patt (type "Rec")))
	(type_decls
		(alias (type "Rec")
			(ty-header (name "Rec"))))
	(expressions
		(expr (type "Rec"))
		(expr (type "Rec"))))
~~~
