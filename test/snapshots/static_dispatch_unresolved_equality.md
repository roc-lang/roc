# META
~~~ini
description=Equality (==) on unresolved type variables is rejected at check time without mentioning is_eq (issue 9485)
type=file
~~~
# SOURCE
~~~roc
poly = || { crash "x" }

result = poly() == poly()
~~~
# EXPECTED
MISSING METHOD - static_dispatch_unresolved_equality.md:3:10:3:16
# PROBLEMS

┌────────────────┐
│ MISSING METHOD ├─ This is trying to compare values with `==`, but their ────┐
└┬───────────────┘  type is an unresolved type variable, which has no         │
 │                  methods.                                                  │
 │                                                                            │
 │  result = poly() == poly()                                                 │
 │           ‾‾‾‾‾‾                                                           │
 └─────────────────────────────── static_dispatch_unresolved_equality.md:3:10 ┘

    Hint: You can replace this static dispatch call with an ordinary function
    call, or force the type variable to become more concrete—for example, by
    adding a type annotation that narrows its type to something that actually
    has methods.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,KwCrash,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,CloseRound,OpEquals,LowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "poly"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-crash
							(e-string
								(e-string-part (raw "x"))))))))
		(s-decl
			(p-ident (raw "result"))
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "poly")))
				(e-apply
					(e-ident (raw "poly")))))))
~~~
# FORMATTED
~~~roc
poly = || {
	crash "x"
}

result = poly() == poly()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "poly"))
		(e-lambda
			(args)
			(e-block
				(e-crash (msg "x")))))
	(d-let
		(p-assign (ident "result"))
		(e-method-eq (negated "false")
			(lhs
				(e-runtime-error (tag "erroneous_value_expr")))
			(rhs
				(e-call (constraint-fn-var 23)
					(e-lookup-local
						(p-assign (ident "poly"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> _ret"))
		(patt (type "Bool")))
	(expressions
		(expr (type "({}) -> _ret"))
		(expr (type "Bool"))))
~~~
