# META
~~~ini
description=A binding whose RHS is an if-expression choosing between two polymorphic functions (expansive, not a bare reference)
type=snippet
~~~
# SOURCE
~~~roc
id = |x| x

picked = if Bool.True id else id

main = (picked(1), picked("a"))
~~~
# EXPECTED
TYPE MISMATCH - generalize_alias_if_branches.md:5:27:5:30
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ This string literal is being used where a non-string ──────┐
└┬──────────────┘  type is needed.                                            │
 │                                                                            │
 │  main = (picked(1), picked("a"))                                           │
 │                            ‾‾‾                                             │
 └────────────────────────────────────── generalize_alias_if_branches.md:5:27 ┘

    The type was determined to be:

        Dec

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,KwIf,UpperIdent,NoSpaceDotUpperIdent,LowerIdent,KwElse,LowerIdent,
LowerIdent,OpAssign,OpenRound,LowerIdent,NoSpaceOpenRound,Int,CloseRound,Comma,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "id"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(s-decl
			(p-ident (raw "picked"))
			(e-if-then-else
				(e-tag (raw "Bool.True"))
				(e-ident (raw "id"))
				(e-ident (raw "id"))))
		(s-decl
			(p-ident (raw "main"))
			(e-tuple
				(e-apply
					(e-ident (raw "picked"))
					(e-int (raw "1")))
				(e-apply
					(e-ident (raw "picked"))
					(e-string
						(e-string-part (raw "a"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "id"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x")))))
	(d-let
		(p-assign (ident "picked"))
		(e-if
			(if-branches
				(if-branch
					(e-nominal-external
						(builtin)
						(e-tag (name "True")))
					(e-lookup-local
						(p-assign (ident "id")))))
			(if-else
				(e-lookup-local
					(p-assign (ident "id"))))))
	(d-let
		(p-assign (ident "main"))
		(e-tuple
			(elems
				(e-call (constraint-fn-var 77)
					(e-lookup-local
						(p-assign (ident "picked")))
					(e-num (value "1")))
				(e-call (constraint-fn-var 94)
					(e-lookup-local
						(p-assign (ident "picked")))
					(e-string
						(e-literal (string "a"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a"))
		(patt (type "Dec -> Dec"))
		(patt (type "(Dec, Dec)")))
	(expressions
		(expr (type "a -> a"))
		(expr (type "Dec -> Dec"))
		(expr (type "(Dec, Dec)"))))
~~~
