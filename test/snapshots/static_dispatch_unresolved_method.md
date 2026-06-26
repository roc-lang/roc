# META
~~~ini
description=Static dispatch of a method on an unresolved type variable is rejected at check time (issue 9485)
type=file
~~~
# SOURCE
~~~roc
poly = || { crash "x" }

result = poly().to_i128()
~~~
# EXPECTED
MISSING METHOD - static_dispatch_unresolved_method.md:3:10:3:16
# PROBLEMS

┌────────────────┐
│ MISSING METHOD ├─ This is trying to dispatch a method named `to_i128` on ───┐
└┬───────────────┘  an unresolved type variable, but unresolved type          │
 │                  variables have no methods.                                │
 │                                                                            │
 │  result = poly().to_i128()                                                 │
 │           ‾‾‾‾‾‾                                                           │
 └───────────────────────────────── static_dispatch_unresolved_method.md:3:10 ┘

    Hint: You can replace this static dispatch call with an ordinary function
    call, or force the type variable to become more concrete—for example, by
    adding a type annotation that narrows its type to something that actually
    has methods.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,KwCrash,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
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
			(e-method-call (method ".to_i128")
				(receiver
					(e-apply
						(e-ident (raw "poly"))))
				(args)))))
~~~
# FORMATTED
~~~roc
poly = || {
	crash "x"
}

result = poly().to_i128()
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
		(e-dispatch-call (method "to_i128") (constraint-fn-var 20)
			(receiver
				(e-runtime-error (tag "erroneous_value_expr")))
			(args))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> _ret"))
		(patt (type "_a")))
	(expressions
		(expr (type "({}) -> _ret"))
		(expr (type "_a"))))
~~~
