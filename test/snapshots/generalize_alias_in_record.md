# META
~~~ini
description=A record field holding a reference to a polymorphic function, used at two types
type=snippet
~~~
# SOURCE
~~~roc
id = |x| x

r = { f: id }

main = (r.f(1), r.f("a"))
~~~
# EXPECTED
TYPE MISMATCH - generalize_alias_in_record.md:5:21:5:24
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ This string literal is being used where a non-string ──────┐
└┬──────────────┘  type is needed.                                            │
 │                                                                            │
 │  main = (r.f(1), r.f("a"))                                                 │
 │                      ‾‾‾                                                   │
 └──────────────────────────────────────── generalize_alias_in_record.md:5:21 ┘

    The type was determined to be:

        Dec

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpAssign,OpenRound,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,Comma,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
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
				(e-method-call (method ".f")
					(receiver
						(e-ident (raw "r")))
					(args
						(e-int (raw "1"))))
				(e-method-call (method ".f")
					(receiver
						(e-ident (raw "r")))
					(args
						(e-string
							(e-string-part (raw "a")))))))))
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
				(e-call (constraint-fn-var 67)
					(e-field-access (field "f")
						(receiver
							(e-lookup-local
								(p-assign (ident "r")))))
					(e-num (value "1")))
				(e-call (constraint-fn-var 87)
					(e-field-access (field "f")
						(receiver
							(e-lookup-local
								(p-assign (ident "r")))))
					(e-string
						(e-literal (string "a"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a"))
		(patt (type "{ f: Dec -> Dec }"))
		(patt (type "(Dec, Dec)")))
	(expressions
		(expr (type "a -> a"))
		(expr (type "{ f: Dec -> Dec }"))
		(expr (type "(Dec, Dec)"))))
~~~
