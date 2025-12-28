# META
~~~ini
description=Simple List.first test without recursive types
type=snippet
~~~
# SOURCE
~~~roc
items : List(I64)
items = [1, 2, 3]

result : I64
result = match List.first(items) {
	Ok(n) => n
	Err(_) => 0
}

expect result == 1
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,KwMatch,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,Int,
CloseCurly,
KwExpect,LowerIdent,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "items")
			(ty-apply
				(ty (name "List"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "items"))
			(e-list
				(e-int (raw "1"))
				(e-int (raw "2"))
				(e-int (raw "3"))))
		(s-type-anno (name "result")
			(ty (name "I64")))
		(s-decl
			(p-ident (raw "result"))
			(e-match
				(e-apply
					(e-ident (raw "List.first"))
					(e-ident (raw "items")))
				(branches
					(branch
						(p-tag (raw "Ok")
							(p-ident (raw "n")))
						(e-ident (raw "n")))
					(branch
						(p-tag (raw "Err")
							(p-underscore))
						(e-int (raw "0"))))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "result"))
				(e-int (raw "1"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "items"))
		(e-list
			(elems
				(e-num (value "1"))
				(e-num (value "2"))
				(e-num (value "3"))))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "I64") (builtin)))))
	(d-let
		(p-assign (ident "result"))
		(e-match
			(match
				(cond
					(e-call
						(e-lookup-external
							(builtin))
						(e-lookup-local
							(p-assign (ident "items")))))
				(branches
					(branch
						(patterns
							(pattern (degenerate false)
								(p-applied-tag)))
						(value
							(e-lookup-local
								(p-assign (ident "n")))))
					(branch
						(patterns
							(pattern (degenerate false)
								(p-applied-tag)))
						(value
							(e-num (value "0")))))))
		(annotation
			(ty-lookup (name "I64") (builtin))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "result")))
			(e-num (value "1")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(I64)"))
		(patt (type "I64")))
	(expressions
		(expr (type "List(I64)"))
		(expr (type "I64"))))
~~~
