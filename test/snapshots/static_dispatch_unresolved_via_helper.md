# META
~~~ini
description=A polymorphic dispatch helper applied to an unresolvable value is rejected; the same helper at a concrete type is accepted (issue 9485)
type=file
~~~
# SOURCE
~~~roc
conv = |x| x.to_i128()

poly = || { crash "x" }

ambiguous = conv(poly())

ok = conv(5.U8)
~~~
# EXPECTED
MISSING METHOD - static_dispatch_unresolved_via_helper.md:5:13:5:25
# PROBLEMS

┌────────────────┐
│ MISSING METHOD ├─ This is trying to dispatch a method named `to_i128` on ───┐
└┬───────────────┘  an unresolved type variable, but unresolved type          │
 │                  variables have no methods.                                │
 │                                                                            │
 │  ambiguous = conv(poly())                                                  │
 │              ‾‾‾‾‾‾‾‾‾‾‾‾                                                  │
 └───────────────────────────── static_dispatch_unresolved_via_helper.md:5:13 ┘

    The type was left undetermined by this call:
      ┌───────────────────────────────────────────────────────────────────────┐
    5 │  ambiguous = conv(poly())                                             │
      │                   ‾‾‾‾‾‾                                              │
      └──────────────────────── static_dispatch_unresolved_via_helper.md:5:18 ┘
    Hint: You can replace this static dispatch call with an ordinary function
    call, or force the type variable to become more concrete—for example, by
    adding a type annotation that narrows its type to something that actually
    has methods.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,KwCrash,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,CloseRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,NoSpaceDotUpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "conv"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-method-call (method ".to_i128")
					(receiver
						(e-ident (raw "x")))
					(args))))
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
			(p-ident (raw "ambiguous"))
			(e-apply
				(e-ident (raw "conv"))
				(e-apply
					(e-ident (raw "poly")))))
		(s-decl
			(p-ident (raw "ok"))
			(e-apply
				(e-ident (raw "conv"))
				(e-typed-int (raw "5") (type "U8"))))))
~~~
# FORMATTED
~~~roc
conv = |x| x.to_i128()

poly = || {
	crash "x"
}

ambiguous = conv(poly())

ok = conv(5.U8)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "conv"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-dispatch-call (method "to_i128") (constraint-fn-var 29)
				(receiver
					(e-lookup-local
						(p-assign (ident "x"))))
				(args))))
	(d-let
		(p-assign (ident "poly"))
		(e-lambda
			(args)
			(e-block
				(e-crash (msg "x")))))
	(d-let
		(p-assign (ident "ambiguous"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "ok"))
		(e-call (constraint-fn-var 78)
			(e-lookup-local
				(p-assign (ident "conv")))
			(e-typed-int (value "5") (type "U8")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> b where [a.to_i128 : a -> b]"))
		(patt (type "({}) -> _ret"))
		(patt (type "_a"))
		(patt (type "I128")))
	(expressions
		(expr (type "a -> b where [a.to_i128 : a -> b]"))
		(expr (type "({}) -> _ret"))
		(expr (type "_a"))
		(expr (type "I128"))))
~~~
