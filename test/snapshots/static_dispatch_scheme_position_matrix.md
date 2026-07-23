# META
~~~ini
description=Pinnability matrix for dispatch-constrained scheme vars: argument and nested-data positions are caller-pinnable (accepted); return-only and constraint-only positions are not (rejected)
type=file
~~~
# SOURCE
~~~roc
via_arg : a -> I128 where [a.to_i128 : a -> I128]
via_arg = |x| x.to_i128()

ok_arg = via_arg(5.U8)

via_data : List(a) -> I128 where [a.to_i128 : a -> I128]
via_data = |xs| {
    match xs {
        [x, ..] => x.to_i128()
        [] => 0
    }
}

ok_data = via_data([5.U8])

gen : {} -> a where [a.gen : {} -> a]
gen = |_| {
    A : a
    A.gen({})
}

unpinned_ret = gen({})

parse_show : Str -> Str where [a.parse : Str -> a, a.show : a -> Str]
parse_show = |s| {
    A : a
    v : a
    v = A.parse(s)
    v.show()
}

roundtrip = parse_show("hi")
~~~
# EXPECTED
POLYMORPHIC VALUE - static_dispatch_scheme_position_matrix.md:22:1:22:13
MISSING METHOD - static_dispatch_scheme_position_matrix.md:19:5:19:14
# PROBLEMS

┌───────────────────┐
│ POLYMORPHIC VALUE ├─ This top-level value still has an unresolved ──────────┐
└┬──────────────────┘  polymorphic type.                                      │
 │                                                                            │
 │  unpinned_ret = gen({})                                                    │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾                                                              │
 └──────────────────────────── static_dispatch_scheme_position_matrix.md:22:1 ┘

    Its type is:
    a where [a.gen : {} -> a]
    Add an annotation or use this value in a way that fixes its concrete type.


┌────────────────┐
│ MISSING METHOD ├─ This is trying to dispatch a method named `gen` on an ────┐
└┬───────────────┘  unresolved type variable, but unresolved type variables   │
 │                  have no methods.                                          │
 │                                                                            │
 │  A.gen({})                                                                 │
 │  ‾‾‾‾‾‾‾‾‾                                                                 │
 └──────────────────────────── static_dispatch_scheme_position_matrix.md:19:5 ┘

    Hint: You can replace this static dispatch call with an ordinary function
    call, or force the type variable to become more concrete—for example, by
    adding a type annotation that narrows its type to something that actually
    has methods.

# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,NoSpaceDotUpperIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,Comma,DoubleDot,CloseSquare,OpFatArrow,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
OpenSquare,CloseSquare,OpFatArrow,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,Int,NoSpaceDotUpperIdent,CloseSquare,CloseRound,
LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
UpperIdent,OpColon,LowerIdent,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,UpperIdent,OpArrow,LowerIdent,Comma,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
UpperIdent,OpColon,LowerIdent,
LowerIdent,OpColon,LowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "via_arg")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "I128")))
			(where
				(method (mod-of "a") (name "to_i128")
					(args
						(ty-var (raw "a")))
					(ty (name "I128")))))
		(s-decl
			(p-ident (raw "via_arg"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-method-call (method ".to_i128")
					(receiver
						(e-ident (raw "x")))
					(args))))
		(s-decl
			(p-ident (raw "ok_arg"))
			(e-apply
				(e-ident (raw "via_arg"))
				(e-typed-int (raw "5") (type "U8"))))
		(s-type-anno (name "via_data")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "a")))
				(ty (name "I128")))
			(where
				(method (mod-of "a") (name "to_i128")
					(args
						(ty-var (raw "a")))
					(ty (name "I128")))))
		(s-decl
			(p-ident (raw "via_data"))
			(e-lambda
				(args
					(p-ident (raw "xs")))
				(e-block
					(statements
						(e-match
							(e-ident (raw "xs"))
							(branches
								(branch
									(p-list
										(p-ident (raw "x"))
										(p-list-rest))
									(e-method-call (method ".to_i128")
										(receiver
											(e-ident (raw "x")))
										(args)))
								(branch
									(p-list)
									(e-int (raw "0")))))))))
		(s-decl
			(p-ident (raw "ok_data"))
			(e-apply
				(e-ident (raw "via_data"))
				(e-list
					(e-typed-int (raw "5") (type "U8")))))
		(s-type-anno (name "gen")
			(ty-fn
				(ty-record)
				(ty-var (raw "a")))
			(where
				(method (mod-of "a") (name "gen")
					(args
						(ty-record))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "gen"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-type-decl
							(header (name "A")
								(args))
							(ty-var (raw "a")))
						(e-apply
							(e-ident (raw "A.gen"))
							(e-record))))))
		(s-decl
			(p-ident (raw "unpinned_ret"))
			(e-apply
				(e-ident (raw "gen"))
				(e-record)))
		(s-type-anno (name "parse_show")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str")))
			(where
				(method (mod-of "a") (name "parse")
					(args
						(ty (name "Str")))
					(ty-var (raw "a")))
				(method (mod-of "a") (name "show")
					(args
						(ty-var (raw "a")))
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "parse_show"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(s-type-decl
							(header (name "A")
								(args))
							(ty-var (raw "a")))
						(s-type-anno (name "v")
							(ty-var (raw "a")))
						(s-decl
							(p-ident (raw "v"))
							(e-apply
								(e-ident (raw "A.parse"))
								(e-ident (raw "s"))))
						(e-method-call (method ".show")
							(receiver
								(e-ident (raw "v")))
							(args))))))
		(s-decl
			(p-ident (raw "roundtrip"))
			(e-apply
				(e-ident (raw "parse_show"))
				(e-string
					(e-string-part (raw "hi")))))))
~~~
# FORMATTED
~~~roc
via_arg : a -> I128 where [a.to_i128 : a -> I128]
via_arg = |x| x.to_i128()

ok_arg = via_arg(5.U8)

via_data : List(a) -> I128 where [a.to_i128 : a -> I128]
via_data = |xs| {
	match xs {
		[x, ..] => x.to_i128()
		[] => 0
	}
}

ok_data = via_data([5.U8])

gen : {} -> a where [a.gen : {} -> a]
gen = |_| {
	A : a
	A.gen({})
}

unpinned_ret = gen({})

parse_show : Str -> Str where [a.parse : Str -> a, a.show : a -> Str]
parse_show = |s| {
	A : a
	v : a
	v = A.parse(s)
	v.show()
}

roundtrip = parse_show("hi")
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "via_arg"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-dispatch-call (method "to_i128") (constraint-fn-var 319)
				(receiver
					(e-lookup-local
						(p-assign (ident "x"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "I128") (builtin)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_i128")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-lookup (name "I128") (builtin))))))
	(d-let
		(p-assign (ident "ok_arg"))
		(e-call (constraint-fn-var 331)
			(e-lookup-local
				(p-assign (ident "via_arg")))
			(e-typed-int (value "5") (type "U8"))))
	(d-let
		(p-assign (ident "via_data"))
		(e-lambda
			(args
				(p-assign (ident "xs")))
			(e-block
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "xs"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-list
											(patterns
												(p-assign (ident "x")))
											(rest-at (index 1)))))
								(value
									(e-dispatch-call (method "to_i128") (constraint-fn-var 347)
										(receiver
											(e-lookup-local
												(p-assign (ident "x"))))
										(args))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-list
											(patterns))))
								(value
									(e-num (value "0")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-rigid-var (name "a")))
				(ty-lookup (name "I128") (builtin)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_i128")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-lookup (name "I128") (builtin))))))
	(d-let
		(p-assign (ident "ok_data"))
		(e-call (constraint-fn-var 382)
			(e-lookup-local
				(p-assign (ident "via_data")))
			(e-list
				(elems
					(e-typed-int (value "5") (type "U8"))))))
	(d-let
		(p-assign (ident "gen"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-type-var-alias (alias "A") (type-var "a")
					(ty-rigid-var (name "a")))
				(e-runtime-error (tag "erroneous_value_expr"))))
		(annotation
			(ty-fn (effectful false)
				(ty-record)
				(ty-rigid-var (name "a")))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "gen")
					(args
						(ty-record))
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "unpinned_ret"))
		(e-call (constraint-fn-var 405)
			(e-lookup-local
				(p-assign (ident "gen")))
			(e-empty_record)))
	(d-let
		(p-assign (ident "parse_show"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(s-type-var-alias (alias "A") (type-var "a")
					(ty-rigid-var (name "a")))
				(s-let
					(p-assign (ident "v"))
					(e-type-dispatch-call (method "parse") (type-dispatch-stmt 88) (constraint-fn-var 422)
						(args
							(e-lookup-local
								(p-assign (ident "s"))))))
				(e-dispatch-call (method "show") (constraint-fn-var 424)
					(receiver
						(e-lookup-local
							(p-assign (ident "v"))))
					(args))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin)))
			(where
				(method (ty-rigid-var (name "a")) (name "parse")
					(args
						(ty-lookup (name "Str") (builtin)))
					(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "show")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "roundtrip"))
		(e-call (constraint-fn-var 434)
			(e-lookup-local
				(p-assign (ident "parse_show")))
			(e-string
				(e-literal (string "hi"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> I128 where [a.to_i128 : a -> I128]"))
		(patt (type "I128"))
		(patt (type "List(a) -> I128 where [a.to_i128 : a -> I128]"))
		(patt (type "I128"))
		(patt (type "{} -> a where [a.gen : {} -> a]"))
		(patt (type "a where [a.gen : {} -> a]"))
		(patt (type "Str -> Str"))
		(patt (type "Str")))
	(expressions
		(expr (type "a -> I128 where [a.to_i128 : a -> I128]"))
		(expr (type "I128"))
		(expr (type "List(a) -> I128 where [a.to_i128 : a -> I128]"))
		(expr (type "I128"))
		(expr (type "{} -> a where [a.gen : {} -> a]"))
		(expr (type "a where [a.gen : {} -> a]"))
		(expr (type "Str -> Str"))
		(expr (type "Str"))))
~~~
