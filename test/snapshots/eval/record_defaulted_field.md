# META
~~~ini
description=Omitted defaulted record field materializes its default at construction
type=snippet
~~~
# SOURCE
~~~roc
my_record : { hello : Str, count : U8 ?? 10 }
my_record = { hello: "hi" }

supplied : { count : U8 ?? 10 }
supplied = { count: 5 }

expect my_record.count == 10
expect supplied.count == 5
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,
KwExpect,LowerIdent,NoSpaceDotLowerIdent,OpEquals,Int,
KwExpect,LowerIdent,NoSpaceDotLowerIdent,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "my_record")
			(ty-record
				(anno-record-field (name "hello")
					(ty (name "Str")))
				(anno-record-field (name "count")
					(ty (name "U8"))
					(default
						(e-int (raw "10"))))))
		(s-decl
			(p-ident (raw "my_record"))
			(e-record
				(field (field "hello")
					(e-string
						(e-string-part (raw "hi"))))))
		(s-type-anno (name "supplied")
			(ty-record
				(anno-record-field (name "count")
					(ty (name "U8"))
					(default
						(e-int (raw "10"))))))
		(s-decl
			(p-ident (raw "supplied"))
			(e-record
				(field (field "count")
					(e-int (raw "5")))))
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
		(e-record
			(fields
				(field (name "hello")
					(e-string
						(e-literal (string "hi"))))))
		(annotation
			(ty-record
				(field (field "hello")
					(ty-lookup (name "Str") (builtin)))
				(field (field "count") (defaulted true)
					(ty-lookup (name "U8") (builtin))))))
	(d-let
		(p-assign (ident "supplied"))
		(e-record
			(fields
				(field (name "count")
					(e-num (value "5")))))
		(annotation
			(ty-record
				(field (field "count") (defaulted true)
					(ty-lookup (name "U8") (builtin))))))
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
		(patt (type "{ count: U8 ?? 10, hello: Str }"))
		(patt (type "{ count: U8 ?? 10 }")))
	(expressions
		(expr (type "{ count: U8 ?? 10, hello: Str }"))
		(expr (type "{ count: U8 ?? 10 }"))))
~~~
