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
NIL
# PROBLEMS
NIL
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
	(type-module)
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
			(e-typed-int (raw "123") (type ".Foo")))))
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
		(e-typed-int (value "123") (type "Foo")))
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
		(patt (type "I64, U8 -> Foo"))
		(patt (type "Foo")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "I64, U8 -> Foo"))
		(expr (type "Foo"))))
~~~
