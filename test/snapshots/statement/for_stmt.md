# META
~~~ini
description=For expression stmt
type=snippet
~~~
# SOURCE
~~~roc
foo : U64
foo = {
	var result = 0
	for x in [1, 2, 3] {
	  result = result + x
	} 
	result
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwFor,LowerIdent,KwIn,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "foo")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "foo"))
			(e-block
				(statements
					(s-var (name "result")
						(e-int (raw "0")))
					(s-for
						(p-ident (raw "x"))
						(e-list
							(e-int (raw "1"))
							(e-int (raw "2"))
							(e-int (raw "3")))
						(e-block
							(statements
								(s-decl
									(p-ident (raw "result"))
									(e-binop (op "+")
										(e-ident (raw "result"))
										(e-ident (raw "x")))))))
					(e-ident (raw "result")))))))
~~~
# FORMATTED
~~~roc
foo : U64
foo = {
	var result = 0
	for x in [1, 2, 3] {
		result = result + x
	}
	result
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-block
			(s-var
				(p-assign (ident "result"))
				(e-num (value "0")))
			(s-for
				(p-assign (ident "x"))
				(e-list
					(elems
						(e-num (value "1"))
						(e-num (value "2"))
						(e-num (value "3"))))
				(e-block
					(s-reassign
						(p-assign (ident "result"))
						(e-binop (op "add")
							(e-lookup-local
								(p-assign (ident "result")))
							(e-lookup-local
								(p-assign (ident "x")))))
					(e-empty_record)))
			(e-lookup-local
				(p-assign (ident "result"))))
		(annotation
			(ty-lookup (name "U64") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(Int(Unsigned64))")))
	(expressions
		(expr (type "Num(Int(Unsigned64))"))))
~~~
