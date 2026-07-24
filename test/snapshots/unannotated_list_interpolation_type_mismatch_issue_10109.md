# META
~~~ini
description=Interpolating a mapped List in an unannotated function reports a type mismatch (issue 10109)
type=file
~~~
# SOURCE
~~~roc
f = |list| {
    inner = list.map(|x| "${x}")
    "<tr>${inner}</tr>"
}

expect f(["a"]) == "x"
~~~
# EXPECTED
TYPE MISMATCH - unannotated_list_interpolation_type_mismatch_issue_10109.md:3:12:3:17
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ This expression is used in an unexpected way. ─────────────┐
└┬──────────────┘                                                             │
 │                                                                            │
 │  "<tr>${inner}</tr>"                                                       │
 │         ‾‾‾‾‾                                                              │
 └────────── unannotated_list_interpolation_type_mismatch_issue_10109.md:3:12 ┘

    It has the type:

        List(b) where [b.from_interpolation : Str, Iter((_field, Str)) -> b]

    But you are trying to use it as:

        Str

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,CloseRound,
StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
CloseCurly,
KwExpect,LowerIdent,NoSpaceOpenRound,OpenSquare,StringStart,StringPart,StringEnd,CloseSquare,CloseRound,OpEquals,StringStart,StringPart,StringEnd,
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
					(p-ident (raw "list")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "inner"))
							(e-method-call (method ".map")
								(receiver
									(e-ident (raw "list")))
								(args
									(e-lambda
										(args
											(p-ident (raw "x")))
										(e-string
											(e-string-part (raw ""))
											(e-ident (raw "x"))
											(e-string-part (raw "")))))))
						(e-string
							(e-string-part (raw "<tr>"))
							(e-ident (raw "inner"))
							(e-string-part (raw "</tr>")))))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "f"))
					(e-list
						(e-string
							(e-string-part (raw "a")))))
				(e-string
					(e-string-part (raw "x")))))))
~~~
# FORMATTED
~~~roc
f = |list| {
	inner = list.map(|x| "${x}")
	"<tr>${inner}</tr>"
}

expect f(["a"]) == "x"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-assign (ident "list")))
			(e-block
				(s-let
					(p-assign (ident "inner"))
					(e-dispatch-call (method "map") (constraint-fn-var 225)
						(receiver
							(e-lookup-local
								(p-assign (ident "list"))))
						(args
							(e-lambda
								(args
									(p-assign (ident "x")))
								(e-block
									(s-let
										(p-assign (ident "#interp_0"))
										(e-lookup-local
											(p-assign (ident "x"))))
									(e-interpolation (constraint-fn-var 223) (dispatcher-var 13)
										(first
											(e-literal (string "")))
										(parts
											(e-lookup-local
												(p-assign (ident "#interp_0")))
											(e-literal (string "")))))))))
				(e-block
					(s-let
						(p-assign (ident "#interp_1"))
						(e-lookup-local
							(p-assign (ident "inner"))))
					(e-interpolation (constraint-fn-var 243) (dispatcher-var 23)
						(first
							(e-literal (string "<tr>")))
						(parts
							(e-lookup-local
								(p-assign (ident "#interp_1")))
							(e-literal (string "</tr>"))))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 269)
					(e-lookup-local
						(p-assign (ident "f")))
					(e-list
						(elems
							(e-string
								(e-literal (string "a")))))))
			(rhs
				(e-string
					(e-literal (string "x")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "c -> d where [c.map : c, (_arg -> e) -> _ret, d.from_interpolation : Str, Iter((_field, Str)) -> d, e.from_interpolation : Str, Iter((_field2, Str)) -> e]")))
	(expressions
		(expr (type "c -> d where [c.map : c, (_arg -> e) -> _ret, d.from_interpolation : Str, Iter((_field, Str)) -> d, e.from_interpolation : Str, Iter((_field2, Str)) -> e]"))))
~~~
