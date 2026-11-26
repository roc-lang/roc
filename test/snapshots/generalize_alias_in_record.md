# META
~~~ini
description=A record field holding a reference to a polymorphic function, used at two types
type=snippet
~~~
# SOURCE
~~~roc
id = |x| x

r = { f: id }

main = ((r.f)(1), (r.f)("a"))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpAssign,OpenRound,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,CloseRound,NoSpaceOpenRound,Int,CloseRound,Comma,OpenRound,LowerIdent,NoSpaceDotLowerIdent,CloseRound,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "id"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(s-decl
			(p-ident (raw "r"))
			(e-record
				(field (field "f")
					(e-ident (raw "id")))))
		(s-decl
			(p-ident (raw "main"))
			(e-tuple
				(e-apply
					(e-tuple
						(e-field-access
							(receiver
								(e-ident (raw "r")))
							(segment (mode "required") (field "f"))))
					(e-int (raw "1")))
				(e-apply
					(e-tuple
						(e-field-access
							(receiver
								(e-ident (raw "r")))
							(segment (mode "required") (field "f"))))
					(e-string
						(e-string-part (raw "a"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "id"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x")))))
	(d-let
		(p-assign (ident "r"))
		(e-record
			(fields
				(field (name "f")
					(e-lookup-local
						(p-assign (ident "id")))))))
	(d-let
		(p-assign (ident "main"))
		(e-tuple
			(elems
				(e-call (constraint-fn-var 238)
					(e-field-access
						(receiver
							(e-lookup-local
								(p-assign (ident "r"))))
						(segments
							(segment (name "f") (mode "required"))))
					(e-num (value "1")))
				(e-call (constraint-fn-var 242)
					(e-field-access
						(receiver
							(e-lookup-local
								(p-assign (ident "r"))))
						(segments
							(segment (name "f") (mode "required"))))
					(e-string
						(e-literal (string "a"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a"))
		(patt (type "{ f: a -> a }"))
		(patt (type "(Dec, Str)")))
	(expressions
		(expr (type "a -> a"))
		(expr (type "{ f: a -> a }"))
		(expr (type "(Dec, Str)"))))
~~~
