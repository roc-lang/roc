# META
~~~ini
description=A chain of bindings that each alias a polymorphic function should stay polymorphic
type=snippet
~~~
# SOURCE
~~~roc
id = |x| x

alias1 = id
alias2 = alias1

main = (alias2(1), alias2("a"))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,LowerIdent,
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
			(p-ident (raw "alias1"))
			(e-ident (raw "id")))
		(s-decl
			(p-ident (raw "alias2"))
			(e-ident (raw "alias1")))
		(s-decl
			(p-ident (raw "main"))
			(e-tuple
				(e-apply
					(e-ident (raw "alias2"))
					(e-int (raw "1")))
				(e-apply
					(e-ident (raw "alias2"))
					(e-string
						(e-string-part (raw "a"))))))))
~~~
# FORMATTED
~~~roc
id = |x| x

alias1 = id

alias2 = alias1

main = (alias2(1), alias2("a"))
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
		(p-assign (ident "alias1"))
		(e-lookup-local
			(p-assign (ident "id"))))
	(d-let
		(p-assign (ident "alias2"))
		(e-lookup-local
			(p-assign (ident "alias1"))))
	(d-let
		(p-assign (ident "main"))
		(e-tuple
			(elems
				(e-call (constraint-fn-var 66)
					(e-lookup-local
						(p-assign (ident "alias2")))
					(e-num (value "1")))
				(e-call (constraint-fn-var 85)
					(e-lookup-local
						(p-assign (ident "alias2")))
					(e-string
						(e-literal (string "a"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a"))
		(patt (type "a -> a"))
		(patt (type "a -> a"))
		(patt (type "(Dec, Str)")))
	(expressions
		(expr (type "a -> a"))
		(expr (type "a -> a"))
		(expr (type "a -> a"))
		(expr (type "(Dec, Str)"))))
~~~
