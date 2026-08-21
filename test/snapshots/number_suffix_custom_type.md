# META
~~~ini
description=Custom type with from_numeral works as number suffix
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Val(I64)].{
  from_numeral : I64, U8 -> Foo
  from_numeral = |n, _| Foo.Val(n)
}

main = 123.Foo
~~~
# EXPECTED
TYPE MISMATCH - number_suffix_custom_type.md:6:8:6:15
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 6 8) (end 6 15))
		(headline
			(reflow "The")
			(reflow " ")
			(annotated code "from_numeral")
			(reflow " ")
			(reflow "method on")
			(reflow " ")
			(annotated code "Foo")
			(reflow " ")
			(reflow "has an incompatible type."))
		(document
			(source-region (file "number_suffix_custom_type.md") (start 6 8) (end 6 15) (annotation error) (line-text "main = 123.Foo"))
			(line-break)
			(reflow "The method")
			(reflow " ")
			(annotated code "from_numeral")
			(reflow " ")
			(reflow "has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "I64, U8 -> Foo")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But I need it to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Numeral -> Try(Foo, [InvalidNumeral(Str)])")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This function expects")
			(reflow " ")
			(reflow "1")
			(reflow " ")
			(reflow "argument")
			(reflow " ")
			(reflow "but got")
			(reflow " ")
			(reflow "2")
			(reflow "."))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,Underscore,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpAssign,Int,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Val"))
						(ty (name "I64")))))
			(associated
				(s-type-anno (name "from_numeral")
					(ty-fn
						(ty (name "I64"))
						(ty (name "U8"))
						(ty (name "Foo"))))
				(s-decl
					(p-ident (raw "from_numeral"))
					(e-lambda
						(args
							(p-ident (raw "n"))
							(p-underscore))
						(e-apply
							(e-tag (raw "Foo.Val"))
							(e-ident (raw "n")))))))
		(s-decl
			(p-ident (raw "main"))
			(e-typed-int (raw "123") (type "Foo")))))
~~~
# FORMATTED
~~~roc
Foo := [Val(I64)].{
	from_numeral : I64, U8 -> Foo
	from_numeral = |n, _| Foo.Val(n)
}

main = 123.Foo
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Foo.from_numeral"))
		(e-lambda
			(args
				(p-assign (ident "n"))
				(p-underscore))
			(e-nominal (nominal "Foo")
				(e-tag (name "Val")
					(args
						(e-lookup-local
							(p-assign (ident "n")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "Foo") (local)))))
	(d-let
		(p-assign (ident "main"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Val")
				(ty-lookup (name "I64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Foo")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "I64, U8 -> Foo"))
		(expr (type "Foo"))))
~~~
