# META
~~~ini
description=A discarded binding that leaves a body-required where-clause output unpinned is rejected at check time instead of crashing at runtime (issues 9815, 9819)
type=file
~~~
# SOURCE
~~~roc
run = || {
    f = |_| Try.Err(NoMore)
    _ = Iter.collect(Iter.custom(0.U64, Unknown, f))
    Ok({})
}
~~~
# EXPECTED
MISSING METHOD - static_dispatch_discarded_iter_issue_9815.md:3:9:3:53
# PROBLEMS

┌────────────────┐
│ MISSING METHOD ├─ This is trying to dispatch a method named `from_iter` ────┐
└┬───────────────┘  on an unresolved type variable, but unresolved type       │
 │                  variables have no methods.                                │
 │                                                                            │
 │  _ = Iter.collect(Iter.custom(0.U64, Unknown, f))                          │
 │      ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                          │
 └────────────────────────── static_dispatch_discarded_iter_issue_9815.md:3:9 ┘

    Hint: You can replace this static dispatch call with an ordinary function
    call, or force the type variable to become more concrete—for example, by
    adding a type annotation that narrows its type to something that actually
    has methods.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
Underscore,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,NoSpaceDotUpperIdent,Comma,UpperIdent,Comma,LowerIdent,CloseRound,CloseRound,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "run"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-decl
							(p-ident (raw "f"))
							(e-lambda
								(args
									(p-underscore))
								(e-apply
									(e-tag (raw "Try.Err"))
									(e-tag (raw "NoMore")))))
						(s-decl
							(p-underscore)
							(e-apply
								(e-ident (raw "Iter.collect"))
								(e-apply
									(e-ident (raw "Iter.custom"))
									(e-typed-int (raw "0") (type "U64"))
									(e-tag (raw "Unknown"))
									(e-ident (raw "f")))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
run = || {
	f = |_| Try.Err(NoMore)
	_ = Iter.collect(Iter.custom(0.U64, Unknown, f))
	Ok({})
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "run"))
		(e-lambda
			(args)
			(e-block
				(s-let
					(p-assign (ident "f"))
					(e-lambda
						(args
							(p-underscore))
						(e-nominal-external
							(builtin)
							(e-tag (name "Err")
								(args
									(e-tag (name "NoMore")))))))
				(s-let
					(p-underscore)
					(e-runtime-error (tag "erroneous_value_expr")))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> [Ok({}), ..]")))
	(expressions
		(expr (type "({}) -> [Ok({}), ..]"))))
~~~
