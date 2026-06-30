# META
~~~ini
description=Nominal-value destructuring (Type.(pat)) on the LHS of a = definition
type=snippet
~~~
# SOURCE
~~~roc
Distance := U64

double : Distance -> U64
double = |d| {
    Distance.(n) = d
    n * 2
}

Distance.(five) = Distance.(5)
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - nominal_destructure_decl.md:5:18:5:19
PARSE ERROR - nominal_destructure_decl.md:9:9:9:10
PARSE ERROR - nominal_destructure_decl.md:9:10:9:11
PARSE ERROR - nominal_destructure_decl.md:9:11:9:15
PARSE ERROR - nominal_destructure_decl.md:9:15:9:16
PARSE ERROR - nominal_destructure_decl.md:9:17:9:18
PARSE ERROR - nominal_destructure_decl.md:9:27:9:28
PARSE ERROR - nominal_destructure_decl.md:9:28:9:29
PARSE ERROR - nominal_destructure_decl.md:9:29:9:30
PARSE ERROR - nominal_destructure_decl.md:9:30:9:31
UNDEFINED VARIABLE - nominal_destructure_decl.md:5:15:5:16
UNRECOGNIZED SYNTAX - nominal_destructure_decl.md:5:18:5:19
UNDEFINED VARIABLE - nominal_destructure_decl.md:6:5:6:6
TYPE MISMATCH - nominal_destructure_decl.md:5:20:5:21
# PROBLEMS

┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token = is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  Distance.(n) = d                                                          │
 │               ‾                                                            │
 └────────────────────────────────────────── nominal_destructure_decl.md:5:18 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  Distance.(five) = Distance.(5)                                            │
 │          ‾                                                                 │
 └─────────────────────────────────────────── nominal_destructure_decl.md:9:9 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Distance.(five) = Distance.(5)                                            │
 │           ‾                                                                │
 └────────────────────────────────────────── nominal_destructure_decl.md:9:10 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Distance.(five) = Distance.(5)                                            │
 │            ‾‾‾‾                                                            │
 └────────────────────────────────────────── nominal_destructure_decl.md:9:11 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Distance.(five) = Distance.(5)                                            │
 │                ‾                                                           │
 └────────────────────────────────────────── nominal_destructure_decl.md:9:15 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Distance.(five) = Distance.(5)                                            │
 │                  ‾                                                         │
 └────────────────────────────────────────── nominal_destructure_decl.md:9:17 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ Type applications require parentheses around their type ─────┐
└┬────────────┘  arguments.                                                   │
 │                                                                            │
 │  Distance.(five) = Distance.(5)                                            │
 │                            ‾                                               │
 └────────────────────────────────────────── nominal_destructure_decl.md:9:27 ┘

    I found a type followed by what looks like a type argument, but they need
    to be connected with parentheses.

    Instead of:
        List U8

    Use:
        List(U8)

    Other valid examples:
        Dict(Str, Num)
        Try(a, Str)
        Maybe(List(U64))


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Distance.(five) = Distance.(5)                                            │
 │                             ‾                                              │
 └────────────────────────────────────────── nominal_destructure_decl.md:9:28 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Distance.(five) = Distance.(5)                                            │
 │                              ‾                                             │
 └────────────────────────────────────────── nominal_destructure_decl.md:9:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  Distance.(five) = Distance.(5)                                            │
 │                               ‾                                            │
 └────────────────────────────────────────── nominal_destructure_decl.md:9:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌────────────────────┐
│ UNDEFINED VARIABLE ├─ Nothing is named `n` in this scope. ──────────────────┐
└┬───────────────────┘                                                        │
 │                                                                            │
 │  Distance.(n) = d                                                          │
 │            ‾                                                               │
 └────────────────────────────────────────── nominal_destructure_decl.md:5:15 ┘

    Is there an `import` or `exposing` missing up-top?


┌─────────────────────┐
│ UNRECOGNIZED SYNTAX ├─ I don't recognize this syntax. ──────────────────────┐
└┬────────────────────┘                                                       │
 │                                                                            │
 │  Distance.(n) = d                                                          │
 │               ‾                                                            │
 └────────────────────────────────────────── nominal_destructure_decl.md:5:18 ┘

    This might be a syntax error, an unsupported language feature, or a typo.


┌────────────────────┐
│ UNDEFINED VARIABLE ├─ Nothing is named `n` in this scope. ──────────────────┐
└┬───────────────────┘                                                        │
 │                                                                            │
 │  n * 2                                                                     │
 │  ‾                                                                         │
 └─────────────────────────────────────────── nominal_destructure_decl.md:6:5 ┘

    Is there an `import` or `exposing` missing up-top?


┌───────────────┐
│ TYPE MISMATCH ├─ This expression produces a value, but it's not being ──────┐
└┬──────────────┘  used.                                                      │
 │                                                                            │
 │  Distance.(n) = d                                                          │
 │                 ‾                                                          │
 └────────────────────────────────────────── nominal_destructure_decl.md:5:20 ┘

    It has the type:

        Distance

    Since this expression is used as a statement, it must evaluate to `{}`.
    If you don't need the value, you can ignore it with `_ =`.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,UpperIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
UpperIdent,Dot,NoSpaceOpenRound,LowerIdent,CloseRound,OpAssign,LowerIdent,
LowerIdent,OpStar,Int,
CloseCurly,
UpperIdent,Dot,NoSpaceOpenRound,LowerIdent,CloseRound,OpAssign,UpperIdent,Dot,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Distance")
				(args))
			(ty (name "U64")))
		(s-type-anno (name "double")
			(ty-fn
				(ty (name "Distance"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "double"))
			(e-lambda
				(args
					(p-ident (raw "d")))
				(e-block
					(statements
						(e-nominal-apply
							(mapper (e-tag (raw "Distance")))
							(e-ident (raw "n")))
						(e-malformed (reason "expr_unexpected_token"))
						(e-ident (raw "d"))
						(e-binop (op "*")
							(e-ident (raw "n"))
							(e-int (raw "2")))))))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
Distance := U64

double : Distance -> U64
double = |d| {
	Distance.(n)
		d
	n * 2
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "double"))
		(e-lambda
			(args
				(p-assign (ident "d")))
			(e-block
				(s-expr
					(e-nominal (nominal "Distance")
						(e-runtime-error (tag "ident_not_in_scope"))))
				(s-expr
					(e-runtime-error (tag "expr_not_canonicalized")))
				(s-expr
					(e-lookup-local
						(p-assign (ident "d"))))
				(e-dispatch-call (method "times") (constraint-fn-var 88)
					(receiver
						(e-runtime-error (tag "ident_not_in_scope")))
					(args
						(e-num (value "2"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Distance") (local))
				(ty-lookup (name "U64") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Distance"))
		(ty-lookup (name "U64") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Distance -> U64")))
	(type_decls
		(nominal (type "Distance")
			(ty-header (name "Distance"))))
	(expressions
		(expr (type "Distance -> U64"))))
~~~
