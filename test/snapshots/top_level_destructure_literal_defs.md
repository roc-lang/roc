# META
~~~ini
description=A top-level destructure of a matching record or tuple literal is the defs it means, so a function among them is as polymorphic as a plainly defined one and nested literals split too
type=file
~~~
# SOURCE
~~~roc
{ same, count } = { same: |x| x, count: 1 }
(twice, base) = (|s| Str.concat(s, s), "ab")
{ outer: { inner }, label } = { outer: { inner: 2 }, label: "n" }
as_str = same("s")
as_num = same(count) + inner
doubled = twice(base)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,OpAssign,OpenCurly,LowerIdent,OpColon,OpBar,LowerIdent,OpBar,LowerIdent,Comma,LowerIdent,OpColon,Int,CloseCurly,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpAssign,OpenRound,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,StringStart,StringPart,StringEnd,CloseRound,
OpenCurly,LowerIdent,OpColon,OpenCurly,LowerIdent,CloseCurly,Comma,LowerIdent,CloseCurly,OpAssign,OpenCurly,LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpPlus,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-record
				(field (name "same") (rest false))
				(field (name "count") (rest false)))
			(e-record
				(field (field "same")
					(e-lambda
						(args
							(p-ident (raw "x")))
						(e-ident (raw "x"))))
				(field (field "count")
					(e-int (raw "1")))))
		(s-decl
			(p-tuple
				(p-ident (raw "twice"))
				(p-ident (raw "base")))
			(e-tuple
				(e-lambda
					(args
						(p-ident (raw "s")))
					(e-apply
						(e-ident (raw "Str.concat"))
						(e-ident (raw "s"))
						(e-ident (raw "s"))))
				(e-string
					(e-string-part (raw "ab")))))
		(s-decl
			(p-record
				(field (name "outer") (rest false)
					(p-record
						(field (name "inner") (rest false))))
				(field (name "label") (rest false)))
			(e-record
				(field (field "outer")
					(e-record
						(field (field "inner")
							(e-int (raw "2")))))
				(field (field "label")
					(e-string
						(e-string-part (raw "n"))))))
		(s-decl
			(p-ident (raw "as_str"))
			(e-apply
				(e-ident (raw "same"))
				(e-string
					(e-string-part (raw "s")))))
		(s-decl
			(p-ident (raw "as_num"))
			(e-binop (op "+")
				(e-apply
					(e-ident (raw "same"))
					(e-ident (raw "count")))
				(e-ident (raw "inner"))))
		(s-decl
			(p-ident (raw "doubled"))
			(e-apply
				(e-ident (raw "twice"))
				(e-ident (raw "base"))))))
~~~
# FORMATTED
~~~roc
{ same, count } = { same: |x| x, count: 1 }

(twice, base) = (|s| Str.concat(s, s), "ab")

{ outer: { inner }, label } = { outer: { inner: 2 }, label: "n" }

as_str = same("s")

as_num = same(count) + inner

doubled = twice(base)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "same"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x")))))
	(d-let
		(p-assign (ident "count"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "twice"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-call (constraint-fn-var 258)
				(e-lookup-external
					(builtin))
				(e-lookup-local
					(p-assign (ident "s")))
				(e-lookup-local
					(p-assign (ident "s"))))))
	(d-let
		(p-assign (ident "base"))
		(e-string
			(e-literal (string "ab"))))
	(d-let
		(p-assign (ident "inner"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "label"))
		(e-string
			(e-literal (string "n"))))
	(d-let
		(p-assign (ident "as_str"))
		(e-call (constraint-fn-var 292)
			(e-lookup-local
				(p-assign (ident "same")))
			(e-string
				(e-literal (string "s")))))
	(d-let
		(p-assign (ident "as_num"))
		(e-dispatch-call (method "plus") (constraint-fn-var 296)
			(receiver
				(e-call (constraint-fn-var 295)
					(e-lookup-local
						(p-assign (ident "same")))
					(e-lookup-local
						(p-assign (ident "count")))))
			(args
				(e-lookup-local
					(p-assign (ident "inner"))))))
	(d-let
		(p-assign (ident "doubled"))
		(e-call (constraint-fn-var 300)
			(e-lookup-local
				(p-assign (ident "twice")))
			(e-lookup-local
				(p-assign (ident "base"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a"))
		(patt (type "Dec"))
		(patt (type "Str -> Str"))
		(patt (type "Str"))
		(patt (type "Dec"))
		(patt (type "Str"))
		(patt (type "Str"))
		(patt (type "Dec"))
		(patt (type "Str")))
	(expressions
		(expr (type "a -> a"))
		(expr (type "Dec"))
		(expr (type "Str -> Str"))
		(expr (type "Str"))
		(expr (type "Dec"))
		(expr (type "Str"))
		(expr (type "Str"))
		(expr (type "Dec"))
		(expr (type "Str"))))
~~~
