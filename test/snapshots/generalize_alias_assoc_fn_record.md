# META
~~~ini
description=A record field holding a reference to a polymorphic associated function carrying a static-dispatch constraint
type=snippet
~~~
# SOURCE
~~~roc
FooBar := {}.{
    myfunc : List(a) -> U64
    myfunc = |list| list.len()
}

bag = { run: FooBar.myfunc }

main = (bag.run([1, 2, 3]), bag.run(["a", "b"]))
~~~
# EXPECTED
MISSING METHOD - generalize_alias_assoc_fn_record.md:8:13:8:16
# PROBLEMS
                                                              ┌────────────────┐
┌─ This run method is being called on a value whose type ─────┤ MISSING METHOD │
│  doesn't have that method:                                  └───────────────┬┘
│                                                                             │
│  main = (bag.run([1, 2, 3]), bag.run(["a", "b"]))                           │
│              ‾‾‾                                                            │
└─────────────────────────────────── generalize_alias_assoc_fn_record.md:8:13 ┘

    The value's type, which does not have a method named run, is:

        { run: List(a) -> U64 }
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceDotLowerIdent,CloseCurly,
LowerIdent,OpAssign,OpenRound,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,CloseRound,Comma,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "FooBar")
				(args))
			(ty-record)
			(associated
				(s-type-anno (name "myfunc")
					(ty-fn
						(ty-apply
							(ty (name "List"))
							(ty-var (raw "a")))
						(ty (name "U64"))))
				(s-decl
					(p-ident (raw "myfunc"))
					(e-lambda
						(args
							(p-ident (raw "list")))
						(e-method-call (method ".len")
							(receiver
								(e-ident (raw "list")))
							(args))))))
		(s-decl
			(p-ident (raw "bag"))
			(e-record
				(field (field "run")
					(e-ident (raw "FooBar.myfunc")))))
		(s-decl
			(p-ident (raw "main"))
			(e-tuple
				(e-method-call (method ".run")
					(receiver
						(e-ident (raw "bag")))
					(args
						(e-list
							(e-int (raw "1"))
							(e-int (raw "2"))
							(e-int (raw "3")))))
				(e-method-call (method ".run")
					(receiver
						(e-ident (raw "bag")))
					(args
						(e-list
							(e-string
								(e-string-part (raw "a")))
							(e-string
								(e-string-part (raw "b"))))))))))
~~~
# FORMATTED
~~~roc
FooBar := {}.{
	myfunc : List(a) -> U64
	myfunc = |list| list.len()
}

bag = { run: FooBar.myfunc }

main = (bag.run([1, 2, 3]), bag.run(["a", "b"]))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "generalize_alias_assoc_fn_record.FooBar.myfunc"))
		(e-lambda
			(args
				(p-assign (ident "list")))
			(e-dispatch-call (method "len") (constraint-fn-var 56)
				(receiver
					(e-lookup-local
						(p-assign (ident "list"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-rigid-var (name "a")))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "bag"))
		(e-record
			(fields
				(field (name "run")
					(e-lookup-local
						(p-assign (ident "generalize_alias_assoc_fn_record.FooBar.myfunc")))))))
	(d-let
		(p-assign (ident "main"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-nominal-decl
		(ty-header (name "FooBar"))
		(ty-record)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(a) -> U64"))
		(patt (type "{ run: List(a) -> U64 }"))
		(patt (type "(Error, Error)")))
	(type_decls
		(nominal (type "FooBar")
			(ty-header (name "FooBar"))))
	(expressions
		(expr (type "List(a) -> U64"))
		(expr (type "{ run: List(a) -> U64 }"))
		(expr (type "(Error, Error)"))))
~~~
