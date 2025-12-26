# META
~~~ini
description=Use break in while loop
type=snippet
~~~
# SOURCE
~~~roc
result : Bool
result = {
	var $foo = True
	while (True) {
		break
		$foo = False
	}	
	$foo
}

expect result == True
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
KwVar,LowerIdent,OpAssign,UpperIdent,
KwWhile,OpenRound,UpperIdent,CloseRound,OpenCurly,
KwBreak,
LowerIdent,OpAssign,UpperIdent,
CloseCurly,
LowerIdent,
CloseCurly,
KwExpect,LowerIdent,OpEquals,UpperIdent,
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
					(s-var (name "$foo")
						(e-tag (raw "True")))
					(s-while
						(e-tuple
							(e-tag (raw "True")))
						(e-block
							(statements
								(s-break)
								(s-decl
									(p-ident (raw "$foo"))
									(e-tag (raw "False"))))))
					(e-ident (raw "$foo")))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "result"))
				(e-tag (raw "True"))))))
~~~
# FORMATTED
~~~roc
result : Bool
result = {
	var $foo = True
	while (True) {
		break
		$foo = False
	}
	$foo
}

expect result == True
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "result"))
		(e-block
			(s-var
				(p-assign (ident "$foo"))
				(e-tag (name "True")))
			(s-while
				(e-tag (name "True"))
				(e-block
					(s-break)
					(s-reassign
						(p-assign (ident "$foo"))
						(e-tag (name "False")))
					(e-empty_record)))
			(e-lookup-local
				(p-assign (ident "$foo"))))
		(annotation
			(ty-lookup (name "Bool") (builtin))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "result")))
			(e-tag (name "True")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Bool")))
	(expressions
		(expr (type "Bool"))))
~~~
