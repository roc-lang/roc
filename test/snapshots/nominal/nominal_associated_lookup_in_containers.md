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
TYPE MISMATCH - nominal_associated_lookup_in_containers.md:7:9:7:18
TYPE MISMATCH - nominal_associated_lookup_in_containers.md:10:10:10:15
TYPE MISMATCH - nominal_associated_lookup_in_containers.md:13:10:13:30
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**nominal_associated_lookup_in_containers.md:7:9:7:18:**
```roc
items = [A, B, C]
```
        ^^^^^^^^^

It has the type:
    _List([A, B, C]_others)_

But the type annotation says it should have the type:
    _List(Foo.Bar)_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**nominal_associated_lookup_in_containers.md:10:10:10:15:**
```roc
result = Ok(A)
```
         ^^^^^

It has the type:
    _Result([A]_others, err)_

But the type annotation says it should have the type:
    _Result(Foo.Bar, Foo.Error)_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**nominal_associated_lookup_in_containers.md:13:10:13:30:**
```roc
nested = { bar: A, count: 1 }
```
         ^^^^^^^^^^^^^^^^^^^^

It has the type:
    _{ bar: [A]_others, count: Num(_size) }_

But the type annotation says it should have the type:
    _{ bar: Foo.Bar, count: Num(Int(Unsigned64)) }_

**Hint:** This might be because the numeric literal is either negative or too large to fit in the unsigned type.

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
			(declared-type
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Foo.Bar") (local))))))
	(d-let
		(p-assign (ident "result"))
		(e-nominal (nominal "Result")
			(e-tag (name "Ok")
				(args
					(e-tag (name "A")))))
		(annotation
			(declared-type
				(ty-apply (name "Result") (local)
					(ty-lookup (name "Foo.Bar") (local))
					(ty-lookup (name "Foo.Error") (local))))))
	(d-let
		(p-assign (ident "nested"))
		(e-record
			(fields
				(field (name "bar")
					(e-tag (name "A")))
				(field (name "count")
					(e-num (value "1")))))
		(annotation
			(declared-type
				(ty-record
					(field (field "bar")
						(ty-lookup (name "Foo.Bar") (local)))
					(field (field "count")
						(ty-lookup (name "U64") (builtin)))))))
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
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "Foo.Bar")))
		(nominal (type "Foo.Error")
			(ty-header (name "Foo.Error"))))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
