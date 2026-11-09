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
UNREACHABLE CODE - break_while_loop.md:6:3:6:15
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unreachable Code")
		(region (start 6 3) (end 6 15))
		(headline
			(reflow "This code will never run, because the code before it always ends with a ")
			(annotated code "return")
			(reflow ", ")
			(annotated code "crash")
			(reflow ", or ")
			(annotated code "break")
			(reflow ", or loops forever."))
		(document
			(source-region (file "break_while_loop.md") (start 6 3) (end 6 15) (annotation error) (line-text "\t\t$foo = False"))
			(reflow "Remove this code, or change the code before it so that it can finish."))))
~~~
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
	(type-mod)
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
				(p-var-assign (ident "$foo"))
				(e-tag (name "True")))
			(s-breakable-loop
				(e-tag (name "True"))
				(e-block
					(s-break)
					(s-reassign
						(p-var-assign (ident "$foo"))
						(e-tag (name "False")))
					(e-empty_record)))
			(e-lookup-local
				(p-var-assign (ident "$foo"))))
		(annotation
			(ty-lookup (name "Bool") (builtin))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-lookup-local
					(p-assign (ident "result"))))
			(rhs
				(e-tag (name "True"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Bool")))
	(expressions
		(expr (type "Bool"))))
~~~
