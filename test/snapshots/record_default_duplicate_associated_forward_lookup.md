# META
~~~ini
description=A forward lookup must not use nested type declarations from a duplicate associated owner. The selected owner has neither Baz nor Qux, so both references are malformed and the duplicate Foo is reported.
type=snippet
~~~
# SOURCE
~~~roc
f : Foo.Baz -> U8
f = |_| 1

g : Foo.Qux -> U8
g = |_| 2

Foo := [A].{
    Bar := { x : U8 }
}

Foo := [B].{
    Baz := { y : U8 }
    Qux : { z : U8 }
}
~~~
# EXPECTED
MISSING NESTED TYPE - record_default_duplicate_associated_forward_lookup.md:1:5:1:12
MISSING NESTED TYPE - record_default_duplicate_associated_forward_lookup.md:4:5:4:12
TYPE REDECLARED - record_default_duplicate_associated_forward_lookup.md:11:1:14:2
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Nested Type")
		(region (start 1 5) (end 1 12))
		(headline
			(annotated code "Foo")
			(reflow " is in scope, but it doesn't have a nested type ")
			(reflow "named ")
			(annotated code "Baz")
			(reflow "."))
		(document
			(source-region (file "record_default_duplicate_associated_forward_lookup.md") (start 1 5) (end 1 12) (annotation error) (line-text "f : Foo.Baz -> U8"))))
	(report
		(severity runtime_error)
		(title "Missing Nested Type")
		(region (start 4 5) (end 4 12))
		(headline
			(annotated code "Foo")
			(reflow " is in scope, but it doesn't have a nested type ")
			(reflow "named ")
			(annotated code "Qux")
			(reflow "."))
		(document
			(source-region (file "record_default_duplicate_associated_forward_lookup.md") (start 4 5) (end 4 12) (annotation error) (line-text "g : Foo.Qux -> U8"))))
	(report
		(severity runtime_error)
		(title "Type Redeclared")
		(region (start 11 1) (end 14 2))
		(headline
			(reflow "The type ")
			(annotated code "Foo")
			(reflow " is being redeclared."))
		(document
			(source-region (file "record_default_duplicate_associated_forward_lookup.md") (start 11 1) (end 14 2) (annotation error) (line-text "Foo := [B].{\n    Baz := { y : U8 }\n    Qux : { z : U8 }\n}"))
			(line-break)
			(reflow "But ")
			(annotated type "Foo")
			(reflow " was already declared in ")
			(source-location
				(file "record_default_duplicate_associated_forward_lookup.md")
				(line 7)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "record_default_duplicate_associated_forward_lookup.md") (start 7 1) (end 9 2) (annotation dim) (line-text "Foo := [A].{\n    Bar := { x : U8 }\n}")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,Int,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "f")
			(ty-fn
				(ty (name "Foo.Baz"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-underscore))
				(e-int (raw "1"))))
		(s-type-anno (name "g")
			(ty-fn
				(ty (name "Foo.Qux"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "g"))
			(e-lambda
				(args
					(p-underscore))
				(e-int (raw "2"))))
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tag-union
				(tags
					(ty (name "A"))))
			(associated
				(s-type-decl
					(header (name "Bar")
						(args))
					(ty-record
						(anno-record-field (name "x")
							(ty (name "U8")))))))
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tag-union
				(tags
					(ty (name "B"))))
			(associated
				(s-type-decl
					(header (name "Baz")
						(args))
					(ty-record
						(anno-record-field (name "y")
							(ty (name "U8")))))
				(s-type-decl
					(header (name "Qux")
						(args))
					(ty-record
						(anno-record-field (name "z")
							(ty (name "U8")))))))))
~~~
# FORMATTED
~~~roc
f : Foo.Baz -> U8
f = |_| 1

g : Foo.Qux -> U8
g = |_| 2

Foo := [A].{
	Bar := { x : U8 }
}

Foo := [B].{
	Baz := { y : U8 }
	Qux : { z : U8 }
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "U8") (builtin)))))
	(d-let
		(p-assign (ident "g"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "U8") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "A"))))
	(s-nominal-decl
		(ty-header (name "record_default_duplicate_associated_forward_lookup.Foo.Bar"))
		(ty-record
			(field (field "x")
				(ty-lookup (name "U8") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "B")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> U8"))
		(patt (type "Error -> U8")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "record_default_duplicate_associated_forward_lookup.Foo.Bar")))
		(nominal (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "Error -> U8"))
		(expr (type "Error -> U8"))))
~~~
