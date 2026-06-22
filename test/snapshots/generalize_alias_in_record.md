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
MISSING METHOD - generalize_alias_in_record.md:5:11:5:12
# PROBLEMS
                                                              ┌────────────────┐
┌─ This f method is being called on a value whose type ───────┤ MISSING METHOD │
│  doesn't have that method:                                  └───────────────┬┘
│                                                                             │
│  main = (r.f(1), r.f("a"))                                                  │
│            ‾                                                                │
└─────────────────────────────────────────────────────────────────────────────┘
    generalize_alias_in_record.md:5:11

    The value's type, which does not have a method named f, is:

        { f: a -> a }
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
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a"))
		(patt (type "{ f: a -> a }"))
		(patt (type "(Error, Error)")))
	(expressions
		(expr (type "a -> a"))
		(expr (type "{ f: a -> a }"))
		(expr (type "(Error, Error)"))))
~~~
