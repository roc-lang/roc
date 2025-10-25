# META
~~~ini
description=Qualified types in type applications (List, Result, etc.)
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [A, B, C]
    Error := [Oops, Yikes]
}

items : List(Foo.Bar)
items = [A, B, C]

result : Result(Foo.Bar, Foo.Error)
result = Ok(A)

nested : { bar : Foo.Bar, count : U64 }
nested = { bar: A, count: 1 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,CloseRound,
LowerIdent,OpAssign,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Whatever"))))
			(associated
				(s-type-decl
					(header (name "Bar")
						(args))
					(ty-tag-union
						(tags
							(ty (name "A"))
							(ty (name "B"))
							(ty (name "C")))))
				(s-type-decl
					(header (name "Error")
						(args))
					(ty-tag-union
						(tags
							(ty (name "Oops"))
							(ty (name "Yikes")))))))
		(s-type-anno (name "items")
			(ty-apply
				(ty (name "List"))
				(ty (name "Foo.Bar"))))
		(s-decl
			(p-ident (raw "items"))
			(e-list
				(e-tag (raw "A"))
				(e-tag (raw "B"))
				(e-tag (raw "C"))))
		(s-type-anno (name "result")
			(ty-apply
				(ty (name "Result"))
				(ty (name "Foo.Bar"))
				(ty (name "Foo.Error"))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-tag (raw "Ok"))
				(e-tag (raw "A"))))
		(s-type-anno (name "nested")
			(ty-record
				(anno-record-field (name "bar")
					(ty (name "Foo.Bar")))
				(anno-record-field (name "count")
					(ty (name "U64")))))
		(s-decl
			(p-ident (raw "nested"))
			(e-record
				(field (field "bar")
					(e-tag (raw "A")))
				(field (field "count")
					(e-int (raw "1")))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [A, B, C]
	Error := [Oops, Yikes]
}

items : List(Foo.Bar)
items = [A, B, C]

result : Result(Foo.Bar, Foo.Error)
result = Ok(A)

nested : { bar : Foo.Bar, count : U64 }
nested = { bar: A, count: 1 }
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "items"))
		(e-list
			(elems
				(e-tag (name "A"))
				(e-tag (name "B"))
				(e-tag (name "C"))))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "Foo.Bar") (local)))))
	(d-let
		(p-assign (ident "result"))
		(e-tag (name "Ok")
			(args
				(e-tag (name "A"))))
		(annotation
			(ty-apply (name "Result") (external-module "Result")
				(ty-lookup (name "Foo.Bar") (local))
				(ty-lookup (name "Foo.Error") (local)))))
	(d-let
		(p-assign (ident "nested"))
		(e-record
			(fields
				(field (name "bar")
					(e-tag (name "A")))
				(field (name "count")
					(e-num (value "1")))))
		(annotation
			(ty-record
				(field (field "bar")
					(ty-lookup (name "Foo.Bar") (local)))
				(field (field "count")
					(ty-lookup (name "U64") (builtin))))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar"))
		(ty-tag-union
			(ty-tag-name (name "A"))
			(ty-tag-name (name "B"))
			(ty-tag-name (name "C"))))
	(s-nominal-decl
		(ty-header (name "Foo.Error"))
		(ty-tag-union
			(ty-tag-name (name "Oops"))
			(ty-tag-name (name "Yikes")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(Foo.Bar)"))
		(patt (type "Result(Foo.Bar, Foo.Error)"))
		(patt (type "{ bar: Foo.Bar, count: Num(Int(Unsigned64)) }")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "Foo.Bar")))
		(nominal (type "Foo.Error")
			(ty-header (name "Foo.Error"))))
	(expressions
		(expr (type "List(Foo.Bar)"))
		(expr (type "Result(Foo.Bar, Foo.Error)"))
		(expr (type "{ bar: Foo.Bar, count: Num(Int(Unsigned64)) }"))))
~~~
