# META
~~~ini
description=For loop iterating over List Bool
type=snippet
~~~
# SOURCE
~~~roc
result : Bool
result = {
	var allTrue_ = Bool.True
	for b in [Bool.True, Bool.True, Bool.False] {
		if b == Bool.False {
			allTrue_ = Bool.False
		} else {
			{}
		}
	}
	allTrue_
}

expect result == Bool.False
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
KwVar,LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
KwFor,LowerIdent,KwIn,OpenSquare,UpperIdent,NoSpaceDotUpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,CloseSquare,OpenCurly,
KwIf,LowerIdent,OpEquals,UpperIdent,NoSpaceDotUpperIdent,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
CloseCurly,KwElse,OpenCurly,
OpenCurly,CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,
CloseCurly,
KwExpect,LowerIdent,OpEquals,UpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "result")
			(ty (name "Bool")))
		(s-decl
			(p-ident (raw "result"))
			(e-block
				(statements
					(s-var (name "allTrue_")
						(e-tag (raw "Bool.True")))
					(s-for
						(p-ident (raw "b"))
						(e-list
							(e-tag (raw "Bool.True"))
							(e-tag (raw "Bool.True"))
							(e-tag (raw "Bool.False")))
						(e-block
							(statements
								(e-if-then-else
									(e-binop (op "==")
										(e-ident (raw "b"))
										(e-tag (raw "Bool.False")))
									(e-block
										(statements
											(s-decl
												(p-ident (raw "allTrue_"))
												(e-tag (raw "Bool.False")))))
									(e-block
										(statements
											(e-record)))))))
					(e-ident (raw "allTrue_")))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "result"))
				(e-tag (raw "Bool.False"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "result"))
		(e-block
			(s-var
				(p-assign (ident "allTrue_"))
				(e-nominal-external
					(external-module "Builtin")
					(e-tag (name "True"))))
			(s-for
				(p-assign (ident "b"))
				(e-list
					(elems
						(e-nominal-external
							(external-module "Builtin")
							(e-tag (name "True")))
						(e-nominal-external
							(external-module "Builtin")
							(e-tag (name "True")))
						(e-nominal-external
							(external-module "Builtin")
							(e-tag (name "False")))))
				(e-block
					(e-if
						(if-branches
							(if-branch
								(e-binop (op "eq")
									(e-lookup-local
										(p-assign (ident "b")))
									(e-nominal-external
										(external-module "Builtin")
										(e-tag (name "False"))))
								(e-block
									(s-reassign
										(p-assign (ident "allTrue_"))
										(e-nominal-external
											(external-module "Builtin")
											(e-tag (name "False"))))
									(e-empty_record))))
						(if-else
							(e-block
								(e-empty_record))))))
			(e-lookup-local
				(p-assign (ident "allTrue_"))))
		(annotation
			(ty-lookup (name "Bool") (external-module "Builtin"))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "result")))
			(e-nominal-external
				(external-module "Builtin")
				(e-tag (name "False"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
