# META
~~~ini
description=Nominal type with is_eq method for equality comparison
type=file:MyColor.roc
~~~
# SOURCE
~~~roc
MyColor := [Red, Green, Blue].{
    is_eq = |a, b| match (a, b) {
        (Red, Red) => 1 == 1
        (Green, Green) => 1 == 1
        (Blue, Blue) => 1 == 1
        _ => 1 == 0
    }
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,KwMatch,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpenCurly,
OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,OpFatArrow,Int,OpEquals,Int,
OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,OpFatArrow,Int,OpEquals,Int,
OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,OpFatArrow,Int,OpEquals,Int,
Underscore,OpFatArrow,Int,OpEquals,Int,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "MyColor")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Red"))
					(ty (name "Green"))
					(ty (name "Blue"))))
			(associated
				(s-decl
					(p-ident (raw "is_eq"))
					(e-lambda
						(args
							(p-ident (raw "a"))
							(p-ident (raw "b")))
						(e-match
							(e-tuple
								(e-ident (raw "a"))
								(e-ident (raw "b")))
							(branches
								(branch
									(p-tuple
										(p-tag (raw "Red"))
										(p-tag (raw "Red")))
									(e-binop (op "==")
										(e-int (raw "1"))
										(e-int (raw "1"))))
								(branch
									(p-tuple
										(p-tag (raw "Green"))
										(p-tag (raw "Green")))
									(e-binop (op "==")
										(e-int (raw "1"))
										(e-int (raw "1"))))
								(branch
									(p-tuple
										(p-tag (raw "Blue"))
										(p-tag (raw "Blue")))
									(e-binop (op "==")
										(e-int (raw "1"))
										(e-int (raw "1"))))
								(branch
									(p-underscore)
									(e-binop (op "==")
										(e-int (raw "1"))
										(e-int (raw "0"))))))))))))
~~~
# FORMATTED
~~~roc
MyColor := [Red, Green, Blue].{
	is_eq = |a, b| match (a, b) {
		(Red, Red) => 1 == 1
		(Green, Green) => 1 == 1
		(Blue, Blue) => 1 == 1
		_ => 1 == 0
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "MyColor.is_eq"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-match
				(match
					(cond
						(e-tuple
							(elems
								(e-lookup-local
									(p-assign (ident "a")))
								(e-lookup-local
									(p-assign (ident "b"))))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-tuple
										(patterns
											(p-applied-tag)
											(p-applied-tag)))))
							(value
								(e-binop (op "eq")
									(e-num (value "1"))
									(e-num (value "1")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-tuple
										(patterns
											(p-applied-tag)
											(p-applied-tag)))))
							(value
								(e-binop (op "eq")
									(e-num (value "1"))
									(e-num (value "1")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-tuple
										(patterns
											(p-applied-tag)
											(p-applied-tag)))))
							(value
								(e-binop (op "eq")
									(e-num (value "1"))
									(e-num (value "1")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-binop (op "eq")
									(e-num (value "1"))
									(e-num (value "0"))))))))))
	(s-nominal-decl
		(ty-header (name "MyColor"))
		(ty-tag-union
			(ty-tag-name (name "Red"))
			(ty-tag-name (name "Green"))
			(ty-tag-name (name "Blue")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[Green, Red, Blue]_others, [Green, Red, Blue]_others2 -> Bool")))
	(type_decls
		(nominal (type "MyColor")
			(ty-header (name "MyColor"))))
	(expressions
		(expr (type "[Green, Red, Blue]_others, [Green, Red, Blue]_others2 -> Bool"))))
~~~
