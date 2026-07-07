# META
~~~ini
description=A where-clause obligation forwards through two generic hops (f -> g -> h) as constraint evidence before resolving at the concrete edge
type=snippet
~~~
# SOURCE
~~~roc
h : a -> Str where [a.describe : a -> Str]
h = |x| x.describe()

g : a -> Str where [a.describe : a -> Str]
g = |x| h(x)

f : a -> Str where [a.describe : a -> Str]
f = |x| g(x)

Named := [N(Str)].{
    describe : Named -> Str
    describe = |Named.N(s)| s
}

main : Str
main = f(Named.N("ok"))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,LowerIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "h")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "Str")))
			(where
				(method (module-of "a") (name "describe")
					(args
						(ty-var (raw "a")))
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "h"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-method-call (method ".describe")
					(receiver
						(e-ident (raw "x")))
					(args))))
		(s-type-anno (name "g")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "Str")))
			(where
				(method (module-of "a") (name "describe")
					(args
						(ty-var (raw "a")))
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "g"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-apply
					(e-ident (raw "h"))
					(e-ident (raw "x")))))
		(s-type-anno (name "f")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "Str")))
			(where
				(method (module-of "a") (name "describe")
					(args
						(ty-var (raw "a")))
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-apply
					(e-ident (raw "g"))
					(e-ident (raw "x")))))
		(s-type-decl
			(header (name "Named")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "N"))
						(ty (name "Str")))))
			(associated
				(s-type-anno (name "describe")
					(ty-fn
						(ty (name "Named"))
						(ty (name "Str"))))
				(s-decl
					(p-ident (raw "describe"))
					(e-lambda
						(args
							(p-tag (raw ".N")
								(p-ident (raw "s"))))
						(e-ident (raw "s"))))))
		(s-type-anno (name "main")
			(ty (name "Str")))
		(s-decl
			(p-ident (raw "main"))
			(e-apply
				(e-ident (raw "f"))
				(e-apply
					(e-tag (raw "Named.N"))
					(e-string
						(e-string-part (raw "ok"))))))))
~~~
# FORMATTED
~~~roc
h : a -> Str where [a.describe : a -> Str]
h = |x| x.describe()

g : a -> Str where [a.describe : a -> Str]
g = |x| h(x)

f : a -> Str where [a.describe : a -> Str]
f = |x| g(x)

Named := [N(Str)].{
	describe : Named -> Str
	describe = |Named.N(s)| s
}

main : Str
main = f(Named.N("ok"))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "h"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-dispatch-call (method "describe") (constraint-fn-var 104)
				(receiver
					(e-lookup-local
						(p-assign (ident "x"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "Str") (builtin)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "describe")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "g"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-call (constraint-fn-var 126)
				(e-lookup-local
					(p-assign (ident "h")))
				(e-lookup-local
					(p-assign (ident "x")))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "Str") (builtin)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "describe")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-call (constraint-fn-var 147)
				(e-lookup-local
					(p-assign (ident "g")))
				(e-lookup-local
					(p-assign (ident "x")))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "Str") (builtin)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "describe")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "where_clause_forwarding_chain_evidence.Named.describe"))
		(e-lambda
			(args
				(p-nominal
					(p-applied-tag)))
			(e-lookup-local
				(p-assign (ident "s"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Named") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "main"))
		(e-call (constraint-fn-var 232)
			(e-lookup-local
				(p-assign (ident "f")))
			(e-nominal (nominal "Named")
				(e-tag (name "N")
					(args
						(e-string
							(e-literal (string "ok")))))))
		(annotation
			(ty-lookup (name "Str") (builtin))))
	(s-nominal-decl
		(ty-header (name "Named"))
		(ty-tag-union
			(ty-tag-name (name "N")
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> Str where [a.describe : a -> Str]"))
		(patt (type "a -> Str where [a.describe : a -> Str]"))
		(patt (type "a -> Str where [a.describe : a -> Str]"))
		(patt (type "Named -> Str"))
		(patt (type "Str")))
	(type_decls
		(nominal (type "Named")
			(ty-header (name "Named"))))
	(expressions
		(expr (type "a -> Str where [a.describe : a -> Str]"))
		(expr (type "a -> Str where [a.describe : a -> Str]"))
		(expr (type "a -> Str where [a.describe : a -> Str]"))
		(expr (type "Named -> Str"))
		(expr (type "Str"))))
~~~
