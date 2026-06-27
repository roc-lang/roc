# META
~~~ini
description=Binops collection
type=expr
~~~
# SOURCE
~~~roc
(
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
    Bool.True and Bool.False,
    Bool.False or Bool.True,
    None ?? 0,
)
~~~
# EXPECTED
TYPE MISMATCH - binops.md:16:5:16:5
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ The first pattern in this `match` is incompatible. ────────┐
└┬──────────────┘                                                             │
 │                                                                            │
 │  None ?? 0,                                                                │
 │  ‾‾‾‾‾‾‾‾‾                                                                 │
 └──────────────────────────────────────────────────────────── binops.md:16:5 ┘

    The first pattern is trying to match:

        Try(ok, err)

    But the expression between the `match` parenthesis has the type:

        [None, ..]

    These can never match! Either the pattern or expression has a problem.

# TOKENS
~~~zig
OpenRound,
Int,OpPlus,Int,Comma,
Int,OpBinaryMinus,Int,Comma,
Int,OpStar,Int,Comma,
Int,OpSlash,Int,Comma,
Int,OpPercent,Int,Comma,
Int,OpLessThan,Int,Comma,
Int,OpGreaterThan,Int,Comma,
Int,OpLessThanOrEq,Int,Comma,
Int,OpGreaterThanOrEq,Int,Comma,
Int,OpEquals,Int,Comma,
Int,OpNotEquals,Int,Comma,
Int,OpDoubleSlash,Int,Comma,
UpperIdent,NoSpaceDotUpperIdent,OpAnd,UpperIdent,NoSpaceDotUpperIdent,Comma,
UpperIdent,NoSpaceDotUpperIdent,OpOr,UpperIdent,NoSpaceDotUpperIdent,Comma,
UpperIdent,OpDoubleQuestion,Int,Comma,
CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple
	(e-binop (op "+")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "-")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "*")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "/")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "%")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "<")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op ">")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "<=")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op ">=")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "==")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "!=")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "//")
		(e-int (raw "4"))
		(e-int (raw "2")))
	(e-binop (op "and")
		(e-tag (raw "Bool.True"))
		(e-tag (raw "Bool.False")))
	(e-binop (op "or")
		(e-tag (raw "Bool.False"))
		(e-tag (raw "Bool.True")))
	(e-binop (op "??")
		(e-tag (raw "None"))
		(e-int (raw "0"))))
~~~
# FORMATTED
~~~roc
(
	4 + 2,
	4 - 2,
	4 * 2,
	4 / 2,
	4 % 2,
	4 < 2,
	4 > 2,
	4 <= 2,
	4 >= 2,
	4 == 2,
	4 != 2,
	4 // 2,
	Bool.True and Bool.False,
	Bool.False or Bool.True,
	None ?? 0,
)
~~~
# CANONICALIZE
~~~clojure
(e-tuple
	(elems
		(e-dispatch-call (method "plus") (constraint-fn-var 139)
			(receiver
				(e-num (value "4")))
			(args
				(e-num (value "2"))))
		(e-dispatch-call (method "minus") (constraint-fn-var 207)
			(receiver
				(e-num (value "4")))
			(args
				(e-num (value "2"))))
		(e-dispatch-call (method "times") (constraint-fn-var 275)
			(receiver
				(e-num (value "4")))
			(args
				(e-num (value "2"))))
		(e-dispatch-call (method "div_by") (constraint-fn-var 343)
			(receiver
				(e-num (value "4")))
			(args
				(e-num (value "2"))))
		(e-dispatch-call (method "rem_by") (constraint-fn-var 411)
			(receiver
				(e-num (value "4")))
			(args
				(e-num (value "2"))))
		(e-dispatch-call (method "is_lt") (constraint-fn-var 482)
			(receiver
				(e-num (value "4")))
			(args
				(e-num (value "2"))))
		(e-dispatch-call (method "is_gt") (constraint-fn-var 553)
			(receiver
				(e-num (value "4")))
			(args
				(e-num (value "2"))))
		(e-dispatch-call (method "is_lte") (constraint-fn-var 624)
			(receiver
				(e-num (value "4")))
			(args
				(e-num (value "2"))))
		(e-dispatch-call (method "is_gte") (constraint-fn-var 695)
			(receiver
				(e-num (value "4")))
			(args
				(e-num (value "2"))))
		(e-method-eq (negated "false")
			(lhs
				(e-num (value "4")))
			(rhs
				(e-num (value "2"))))
		(e-method-eq (negated "true")
			(lhs
				(e-num (value "4")))
			(rhs
				(e-num (value "2"))))
		(e-dispatch-call (method "div_trunc_by") (constraint-fn-var 915)
			(receiver
				(e-num (value "4")))
			(args
				(e-num (value "2"))))
		(e-if
			(if-branches
				(if-branch
					(e-nominal-external
						(builtin)
						(e-tag (name "True")))
					(e-nominal-external
						(builtin)
						(e-tag (name "False")))))
			(if-else
				(e-nominal-external
					(builtin)
					(e-tag (name "False")))))
		(e-if
			(if-branches
				(if-branch
					(e-nominal-external
						(builtin)
						(e-tag (name "False")))
					(e-nominal-external
						(builtin)
						(e-tag (name "True")))))
			(if-else
				(e-nominal-external
					(builtin)
					(e-tag (name "True")))))
		(e-match
			(match
				(cond
					(e-tag (name "None")))
				(branches
					(branch
						(patterns
							(pattern (degenerate false)
								(p-nominal-external (builtin)
									(p-applied-tag))))
						(value
							(e-lookup-local
								(p-assign (ident "#ok")))))
					(branch
						(patterns
							(pattern (degenerate false)
								(p-nominal-external (builtin)
									(p-applied-tag))))
						(value
							(e-num (value "0")))))))))
~~~
# TYPES
~~~clojure
(expr (type "(Dec, Dec, Dec, Dec, Dec, Bool, Bool, Bool, Bool, Bool, Bool, Dec, Bool, Bool, Dec)"))
~~~
