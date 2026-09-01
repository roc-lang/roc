# META
~~~ini
description=Omitted defaulted record field materializes its default at construction
type=snippet
~~~
# SOURCE
~~~roc
MyRecord := { hello : Str, count : U8 ?? 10 }

my_record : MyRecord
my_record = MyRecord.{ hello: "hi" }

Supplied := { count : U8 ?? 10 }

supplied : Supplied
supplied = Supplied.{ count: 5 }

expect my_record.count == 10
expect supplied.count == 5
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,
KwExpect,LowerIdent,NoSpaceDotLowerIdent,OpEquals,Int,
KwExpect,LowerIdent,NoSpaceDotLowerIdent,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "MyRecord")
				(args))
			(ty-record
				(anno-record-field (name "hello")
					(ty (name "Str")))
				(anno-record-field (name "count")
					(ty (name "U8"))
					(default
						(e-int (raw "10"))))))
		(s-type-anno (name "my_record")
			(ty (name "MyRecord")))
		(s-decl
			(p-ident (raw "my_record"))
			(e-nominal-record
				(mapper (e-tag (raw "MyRecord")))
				(backing (e-record
						(field (field "hello")
							(e-string
								(e-string-part (raw "hi"))))))))
		(s-type-decl
			(header (name "Supplied")
				(args))
			(ty-record
				(anno-record-field (name "count")
					(ty (name "U8"))
					(default
						(e-int (raw "10"))))))
		(s-type-anno (name "supplied")
			(ty (name "Supplied")))
		(s-decl
			(p-ident (raw "supplied"))
			(e-nominal-record
				(mapper (e-tag (raw "Supplied")))
				(backing (e-record
						(field (field "count")
							(e-int (raw "5")))))))
		(s-expect
			(e-binop (op "==")
				(e-field-access
					(receiver
						(e-ident (raw "my_record")))
					(segment (mode "required") (field "count")))
				(e-int (raw "10"))))
		(s-expect
			(e-binop (op "==")
				(e-field-access
					(receiver
						(e-ident (raw "supplied")))
					(segment (mode "required") (field "count")))
				(e-int (raw "5"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "my_record"))
		(e-nominal (nominal "MyRecord")
			(e-record
				(fields
					(field (name "hello")
						(e-string
							(e-literal (string "hi")))))))
		(annotation
			(ty-lookup (name "MyRecord") (local))))
	(d-let
		(p-assign (ident "supplied"))
		(e-nominal (nominal "Supplied")
			(e-record
				(fields
					(field (name "count")
						(e-num (value "5"))))))
		(annotation
			(ty-lookup (name "Supplied") (local))))
	(s-nominal-decl
		(ty-header (name "MyRecord"))
		(ty-record
			(field (field "hello")
				(ty-lookup (name "Str") (builtin)))
			(field (field "count") (defaulted true)
				(ty-lookup (name "U8") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Supplied"))
		(ty-record
			(field (field "count") (defaulted true)
				(ty-lookup (name "U8") (builtin)))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-field-access
					(receiver
						(e-lookup-local
							(p-assign (ident "my_record"))))
					(segments
						(segment (name "count") (mode "required")))))
			(rhs
				(e-num (value "10")))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-field-access
					(receiver
						(e-lookup-local
							(p-assign (ident "supplied"))))
					(segments
						(segment (name "count") (mode "required")))))
			(rhs
				(e-num (value "5"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "MyRecord"))
		(patt (type "Supplied")))
	(type_decls
		(nominal (type "MyRecord")
			(ty-header (name "MyRecord")))
		(nominal (type "Supplied")
			(ty-header (name "Supplied"))))
	(expressions
		(expr (type "MyRecord"))
		(expr (type "Supplied"))))
~~~
