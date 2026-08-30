# META
~~~ini
description=Deduplicates same-name method requirements whose callable differences are private to the inferred function
type=file
~~~
# SOURCE
~~~roc
f = |value| {
    _first = value.convert()
    _second = value.convert()

    {}
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
NamedUnderscore,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
NamedUnderscore,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
OpenCurly,CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
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
								(args)))
						(s-decl
							(p-ident (raw "_second"))
							(e-method-call (method ".convert")
								(receiver
									(e-ident (raw "value")))
								(args)))
						(e-record)))))))
~~~
# FORMATTED
~~~roc
f = |value| {
	_first = value.convert()
	_second = value.convert()

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
					(e-dispatch-call (method "convert") (constraint-fn-var 214)
						(receiver
							(e-lookup-local
								(p-assign (ident "value"))))
						(args)))
				(s-let
					(p-assign (ident "_second"))
					(e-dispatch-call (method "convert") (constraint-fn-var 216)
						(receiver
							(e-lookup-local
								(p-assign (ident "value"))))
						(args)))
				(e-empty_record)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> {} where [a.convert : a -> _ret]")))
	(expressions
		(expr (type "a -> {} where [a.convert : a -> _ret]"))))
~~~
