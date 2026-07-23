# META
~~~ini
description=Discharging a where-clause on a concrete type validates the chosen target's own nested where-clauses (issue 9892)
type=snippet
~~~
# SOURCE
~~~roc
Wrap(a) := [W(a)].{
    unwrap : Wrap(a) -> Str where [a.frobnicate : a -> Str]
    unwrap = |Wrap.W(x)| x.frobnicate()
}

run : b -> Str where [b.unwrap : b -> Str]
run = |v| v.unwrap()

main : Str
main = run(Wrap.W(42.U8))
~~~
# EXPECTED
MISSING METHOD - where_clause_nested_obligation_missing_method_issue_9892.md:3:28:3:38
# PROBLEMS

┌────────────────┐
│ MISSING METHOD ├─ This `frobnicate` method is being called on a value ──────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  unwrap = |Wrap.W(x)| x.frobnicate()                                       │
 │                         ‾‾‾‾‾‾‾‾‾‾                                         │
 └────────── where_clause_nested_obligation_missing_method_issue_9892.md:3:28 ┘

    The value's type, which does not have a method named `frobnicate`, is:

        U8

    Hint: For this to work, the type would need to have a method named
    `frobnicate` associated with it in the type's declaration.

# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,NoSpaceDotUpperIdent,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Wrap")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "W"))
						(ty-var (raw "a")))))
			(associated
				(s-type-anno (name "unwrap")
					(ty-fn
						(ty-apply
							(ty (name "Wrap"))
							(ty-var (raw "a")))
						(ty (name "Str")))
					(where
						(method (mod-of "a") (name "frobnicate")
							(args
								(ty-var (raw "a")))
							(ty (name "Str")))))
				(s-decl
					(p-ident (raw "unwrap"))
					(e-lambda
						(args
							(p-tag (raw ".W")
								(p-ident (raw "x"))))
						(e-method-call (method ".frobnicate")
							(receiver
								(e-ident (raw "x")))
							(args))))))
		(s-type-anno (name "run")
			(ty-fn
				(ty-var (raw "b"))
				(ty (name "Str")))
			(where
				(method (mod-of "b") (name "unwrap")
					(args
						(ty-var (raw "b")))
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "run"))
			(e-lambda
				(args
					(p-ident (raw "v")))
				(e-method-call (method ".unwrap")
					(receiver
						(e-ident (raw "v")))
					(args))))
		(s-type-anno (name "main")
			(ty (name "Str")))
		(s-decl
			(p-ident (raw "main"))
			(e-apply
				(e-ident (raw "run"))
				(e-apply
					(e-tag (raw "Wrap.W"))
					(e-typed-int (raw "42") (type "U8")))))))
~~~
# FORMATTED
~~~roc
Wrap(a) := [W(a)].{
	unwrap : Wrap(a) -> Str where [a.frobnicate : a -> Str]
	unwrap = |Wrap.W(x)| x.frobnicate()
}

run : b -> Str where [b.unwrap : b -> Str]
run = |v| v.unwrap()

main : Str
main = run(Wrap.W(42.U8))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "where_clause_nested_obligation_missing_method_issue_9892.Wrap.unwrap"))
		(e-lambda
			(args
				(p-nominal
					(p-applied-tag)))
			(e-dispatch-call (method "frobnicate") (constraint-fn-var 259)
				(receiver
					(e-lookup-local
						(p-assign (ident "x"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Wrap") (local)
					(ty-rigid-var (name "a")))
				(ty-lookup (name "Str") (builtin)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "frobnicate")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "run"))
		(e-lambda
			(args
				(p-assign (ident "v")))
			(e-dispatch-call (method "unwrap") (constraint-fn-var 270)
				(receiver
					(e-lookup-local
						(p-assign (ident "v"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "b"))
				(ty-lookup (name "Str") (builtin)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "b"))) (name "unwrap")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "b"))))
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "main"))
		(e-call (constraint-fn-var 290)
			(e-lookup-local
				(p-assign (ident "run")))
			(e-nominal (nominal "Wrap")
				(e-tag (name "W")
					(args
						(e-typed-int (value "42") (type "U8"))))))
		(annotation
			(ty-lookup (name "Str") (builtin))))
	(s-nominal-decl
		(ty-header (name "Wrap")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "W")
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Wrap(a) -> Error where [a.frobnicate : a -> Error]"))
		(patt (type "b -> Error where [b.unwrap : b -> Error]"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "Wrap(a)")
			(ty-header (name "Wrap")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "Wrap(a) -> Error where [a.frobnicate : a -> Error]"))
		(expr (type "b -> Error where [b.unwrap : b -> Error]"))
		(expr (type "Error"))))
~~~
