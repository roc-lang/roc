# META
~~~ini
description=Checks every same-name body call against the callable type promised by a where requirement
type=file
~~~
# SOURCE
~~~roc
f : a -> {} where [a.convert : a, Str -> U64]
f = |value| {
    _first = value.convert("ok")
    _second = value.convert(1.U64)

    {}
}
~~~
# EXPECTED
TYPE MISMATCH - where_requirement_checks_every_same_method_call.md:4:21:4:28
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 4 21) (end 4 28))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "where_requirement_checks_every_same_method_call.md") (start 4 21) (end 4 28) (annotation error) (line-text "    _second = value.convert(1.U64)"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a, U64 -> _ret where [a.convert : a, Str -> U64]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But you are trying to use it as:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a, Str -> U64 where [a.convert : a, Str -> U64]")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,OpArrow,OpenCurly,CloseCurly,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,Comma,UpperIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
NamedUnderscore,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
NamedUnderscore,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,NoSpaceDotUpperIdent,CloseRound,
OpenCurly,CloseCurly,
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
				(ty-var (raw "a"))
				(ty-record))
			(where
				(method (mod-of "a") (name "convert")
					(ty-fn
						(ty-var (raw "a"))
						(ty (name "Str"))
						(ty (name "U64"))))))
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "_first"))
							(e-method-call (method ".convert")
								(receiver
									(e-ident (raw "value")))
								(args
									(e-string
										(e-string-part (raw "ok"))))))
						(s-decl
							(p-ident (raw "_second"))
							(e-method-call (method ".convert")
								(receiver
									(e-ident (raw "value")))
								(args
									(e-typed-int (raw "1") (type "U64")))))
						(e-record)))))))
~~~
# FORMATTED
~~~roc
f : a -> {} where [a.convert : a, Str -> U64]
f = |value| {
	_first = value.convert("ok")
	_second = value.convert(1.U64)

	{}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-block
				(s-let
					(p-assign (ident "_first"))
					(e-dispatch-call (method "convert") (constraint-fn-var 255)
						(receiver
							(e-lookup-local
								(p-assign (ident "value"))))
						(args
							(e-string
								(e-literal (string "ok"))))))
				(s-let
					(p-assign (ident "_second"))
					(e-runtime-error (tag "erroneous_value_expr")))
				(e-empty_record)))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-record))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "convert")
					(ty-fn (effectful false)
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-lookup (name "Str") (builtin))
						(ty-lookup (name "U64") (builtin))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> {} where [a.convert : Error]")))
	(expressions
		(expr (type "a -> {} where [a.convert : Error]"))))
~~~
