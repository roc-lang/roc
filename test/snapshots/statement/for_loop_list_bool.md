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
VAR NAME MISSING `$` - for_loop_list_bool.md:3:6:3:14
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Var Name Missing `$`")
		(region (start 3 6) (end 3 14))
		(headline
			(reflow "The mutable binding ")
			(annotated symbol-unqualified "allTrue_")
			(reflow " is declared with ")
			(annotated keyword "var")
			(reflow " but its name does not start with ")
			(annotated code "$")
			(reflow "."))
		(document
			(reflow "Rename this binding and all of its uses to ")
			(annotated symbol-unqualified "$allTrue_")
			(reflow ". The name is only a convention; mutability comes from the ")
			(annotated keyword "var")
			(reflow " declaration.")
			(line-break)
			(line-break)
			(source-region (file "for_loop_list_bool.md") (start 3 6) (end 3 14) (annotation warning) (line-text "\tvar allTrue_ = Bool.True")))))
~~~
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
	(type-mod)
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
				(p-var-assign (ident "allTrue_"))
				(e-nominal-external
					(builtin)
					(e-tag (name "True"))))
			(s-for
				(p-assign (ident "b"))
				(e-list
					(elems
						(e-nominal-external
							(builtin)
							(e-tag (name "True")))
						(e-nominal-external
							(builtin)
							(e-tag (name "True")))
						(e-nominal-external
							(builtin)
							(e-tag (name "False")))))
				(e-block
					(e-if
						(if-branches
							(if-branch
								(e-method-eq (negated "false")
									(lhs
										(e-lookup-local
											(p-assign (ident "b"))))
									(rhs
										(e-nominal-external
											(builtin)
											(e-tag (name "False")))))
								(e-block
									(s-reassign
										(p-var-assign (ident "allTrue_"))
										(e-nominal-external
											(builtin)
											(e-tag (name "False"))))
									(e-empty_record))))
						(if-else
							(e-block
								(e-empty_record))))))
			(e-lookup-local
				(p-var-assign (ident "allTrue_"))))
		(annotation
			(ty-lookup (name "Bool") (builtin))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-lookup-local
					(p-assign (ident "result"))))
			(rhs
				(e-nominal-external
					(builtin)
					(e-tag (name "False")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Bool")))
	(expressions
		(expr (type "Bool"))))
~~~
