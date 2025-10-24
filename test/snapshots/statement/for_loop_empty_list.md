# META
~~~ini
description=For loop with empty list
type=snippet
~~~
# SOURCE
~~~roc
unchanged : U64
unchanged = {
	var value_ = 42
	for n in [] {
		value_ = n
	}
	value_
}

expect unchanged == 42
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
KwFor,LowerIdent,KwIn,OpenSquare,CloseSquare,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,
LowerIdent,
CloseCurly,
KwExpect,LowerIdent,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "unchanged")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "unchanged"))
			(e-block
				(statements
					(s-var (name "value_")
						(e-int (raw "42")))
					(s-for
						(p-ident (raw "n"))
						(e-list)
						(e-block
							(statements
								(s-decl
									(p-ident (raw "value_"))
									(e-ident (raw "n"))))))
					(e-ident (raw "value_")))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "unchanged"))
				(e-int (raw "42"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "unchanged"))
		(e-block
			(s-var
				(p-assign (ident "value_"))
				(e-num (value "42")))
			(s-for
				(p-assign (ident "n"))
				(e-empty_list)
				(e-block
					(s-reassign
						(p-assign (ident "value_"))
						(e-lookup-local
							(p-assign (ident "n"))))
					(e-empty_record)))
			(e-lookup-local
				(p-assign (ident "value_"))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "unchanged")))
			(e-num (value "42")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(Int(Unsigned64))")))
	(expressions
		(expr (type "Num(Int(Unsigned64))"))))
~~~
