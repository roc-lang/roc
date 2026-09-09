# META
~~~ini
description=Names bound by a top-level destructure are ordinary top-level names: other defs may use them, before or after the destructure, and a destructured function is callable
type=file
~~~
# SOURCE
~~~roc
earlier = x + 1
{x} = { x: 1 }
later = x + 2
{f, n} = { f: |v| Str.concat(v, n), n: "s" }
greeting = f("hello ")
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
OpenCurly,LowerIdent,CloseCurly,OpAssign,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,OpAssign,OpenCurly,LowerIdent,OpColon,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "earlier"))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-int (raw "1"))))
		(s-decl
			(p-record
				(field (name "x") (rest false)))
			(e-record
				(field (field "x")
					(e-int (raw "1")))))
		(s-decl
			(p-ident (raw "later"))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-int (raw "2"))))
		(s-decl
			(p-record
				(field (name "f") (rest false))
				(field (name "n") (rest false)))
			(e-record
				(field (field "f")
					(e-lambda
						(args
							(p-ident (raw "v")))
						(e-apply
							(e-ident (raw "Str.concat"))
							(e-ident (raw "v"))
							(e-ident (raw "n")))))
				(field (field "n")
					(e-string
						(e-string-part (raw "s"))))))
		(s-decl
			(p-ident (raw "greeting"))
			(e-apply
				(e-ident (raw "f"))
				(e-string
					(e-string-part (raw "hello ")))))))
~~~
# FORMATTED
~~~roc
earlier = x + 1

{ x } = { x: 1 }

later = x + 2

{ f, n } = { f: |v| Str.concat(v, n), n: "s" }

greeting = f("hello ")
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "earlier"))
		(e-dispatch-call (method "plus") (constraint-fn-var 245)
			(receiver
				(e-lookup-local
					(p-assign (ident "x"))))
			(args
				(e-num (value "1")))))
	(d-let
		(p-assign (ident "x"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "later"))
		(e-dispatch-call (method "plus") (constraint-fn-var 254)
			(receiver
				(e-lookup-local
					(p-assign (ident "x"))))
			(args
				(e-num (value "2")))))
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-assign (ident "v")))
			(e-call (constraint-fn-var 270)
				(e-lookup-external
					(builtin))
				(e-lookup-local
					(p-assign (ident "v")))
				(e-lookup-local
					(p-assign (ident "n"))))))
	(d-let
		(p-assign (ident "n"))
		(e-string
			(e-literal (string "s"))))
	(d-let
		(p-assign (ident "greeting"))
		(e-call (constraint-fn-var 281)
			(e-lookup-local
				(p-assign (ident "f")))
			(e-string
				(e-literal (string "hello "))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "Dec"))
		(patt (type "Str -> Str"))
		(patt (type "Str"))
		(patt (type "Str")))
	(expressions
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "Dec"))
		(expr (type "Str -> Str"))
		(expr (type "Str"))
		(expr (type "Str"))))
~~~
