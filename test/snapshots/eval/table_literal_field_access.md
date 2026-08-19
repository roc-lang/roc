# META
~~~ini
description=Table literal desugars to a list of records
type=snippet
~~~
# SOURCE
~~~roc
people = table name : Str, age : U8 {
    "Bob", 12,
    "Alice", 17,
}

first = match List.first(people) {
    Ok(person) => person
    Err(_) => { name: "", age: 0 }
}

expect first.name == "Bob"
expect first.age == 12
expect people.len() == 2
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,LowerIdent,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,OpenCurly,
StringStart,StringPart,StringEnd,Comma,Int,Comma,
StringStart,StringPart,StringEnd,Comma,Int,Comma,
CloseCurly,
LowerIdent,OpAssign,KwMatch,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,CloseCurly,
CloseCurly,
KwExpect,LowerIdent,NoSpaceDotLowerIdent,OpEquals,StringStart,StringPart,StringEnd,
KwExpect,LowerIdent,NoSpaceDotLowerIdent,OpEquals,Int,
KwExpect,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "people"))
			(e-table
				(columns
					(table-column (name "name")
						(ty (name "Str")))
					(table-column (name "age")
						(ty (name "U8"))))
				(rows
					(table-row
						(e-string
							(e-string-part (raw "Bob")))
						(e-int (raw "12")))
					(table-row
						(e-string
							(e-string-part (raw "Alice")))
						(e-int (raw "17"))))))
		(s-decl
			(p-ident (raw "first"))
			(e-match
				(e-apply
					(e-ident (raw "List.first"))
					(e-ident (raw "people")))
				(branches
					(branch
						(p-tag (raw "Ok")
							(p-ident (raw "person")))
						(e-ident (raw "person")))
					(branch
						(p-tag (raw "Err")
							(p-underscore))
						(e-record
							(field (field "name")
								(e-string
									(e-string-part (raw ""))))
							(field (field "age")
								(e-int (raw "0"))))))))
		(s-expect
			(e-binop (op "==")
				(e-field-access
					(receiver
						(e-ident (raw "first")))
					(segment (mode "required") (field "name")))
				(e-string
					(e-string-part (raw "Bob")))))
		(s-expect
			(e-binop (op "==")
				(e-field-access
					(receiver
						(e-ident (raw "first")))
					(segment (mode "required") (field "age")))
				(e-int (raw "12"))))
		(s-expect
			(e-binop (op "==")
				(e-method-call (method ".len")
					(receiver
						(e-ident (raw "people")))
					(args))
				(e-int (raw "2"))))))
~~~
# FORMATTED
~~~roc
people = table name : Str, age : U8 {
	"Bob", 12,
	"Alice", 17,
}

first = match List.first(people) {
	Ok(person) => person
	Err(_) => { name: "", age: 0 }
}

expect first.name == "Bob"
expect first.age == 12
expect people.len() == 2
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "people"))
		(e-block
			(s-let
				(p-assign (ident "#1"))
				(e-list
					(elems
						(e-record
							(fields
								(field (name "name")
									(e-string
										(e-literal (string "Bob"))))
								(field (name "age")
									(e-num (value "12")))))
						(e-record
							(fields
								(field (name "name")
									(e-string
										(e-literal (string "Alice"))))
								(field (name "age")
									(e-num (value "17"))))))))
			(e-lookup-local
				(p-assign (ident "#1")))))
	(d-let
		(p-assign (ident "first"))
		(e-match
			(match
				(cond
					(e-call (constraint-fn-var 333)
						(e-lookup-external
							(builtin))
						(e-lookup-local
							(p-assign (ident "people")))))
				(branches
					(branch
						(patterns
							(pattern (degenerate false)
								(p-applied-tag)))
						(value
							(e-lookup-local
								(p-assign (ident "person")))))
					(branch
						(patterns
							(pattern (degenerate false)
								(p-applied-tag)))
						(value
							(e-record
								(fields
									(field (name "name")
										(e-string
											(e-literal (string ""))))
									(field (name "age")
										(e-num (value "0")))))))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-field-access
					(receiver
						(e-lookup-local
							(p-assign (ident "first"))))
					(segments
						(segment (name "name") (mode "required")))))
			(rhs
				(e-string
					(e-literal (string "Bob"))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-field-access
					(receiver
						(e-lookup-local
							(p-assign (ident "first"))))
					(segments
						(segment (name "age") (mode "required")))))
			(rhs
				(e-num (value "12")))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-dispatch-call (method "len") (constraint-fn-var 409)
					(receiver
						(e-lookup-local
							(p-assign (ident "people"))))
					(args)))
			(rhs
				(e-num (value "2"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List({ age: U8, name: Str })"))
		(patt (type "{ age: U8, name: Str }")))
	(expressions
		(expr (type "List({ age: U8, name: Str })"))
		(expr (type "{ age: U8, name: Str }"))))
~~~
