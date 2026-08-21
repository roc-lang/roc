# META
~~~ini
description=Variable scoping with var keyword
type=snippet
~~~
# SOURCE
~~~roc
# Test var redeclaration (should produce shadowing warning)
redeclareTest = |_| {
	var x_ = 5
	var x_ = 10 # Redeclare var - should warn but proceed
	x_ = 15 # Reassign - should work without warning
	x_
}

result = redeclareTest({})
~~~
# EXPECTED
DUPLICATE DEFINITION - can_var_scoping_var_redeclaration.md:4:2:4:13
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 4 2) (end 4 13))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "x_")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "can_var_scoping_var_redeclaration.md") (start 4 2) (end 4 13) (annotation error) (line-text "\tvar x_ = 10 # Redeclare var - should warn but proceed"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "x_")
			(reflow " was already defined in ")
			(source-location
				(file "can_var_scoping_var_redeclaration.md")
				(line 3)
				(column 2))
			(reflow ":")
			(line-break)
			(source-region (file "can_var_scoping_var_redeclaration.md") (start 3 2) (end 3 12) (annotation dim) (line-text "\tvar x_ = 5")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwVar,LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "redeclareTest"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-var (name "x_")
							(e-int (raw "5")))
						(s-var (name "x_")
							(e-int (raw "10")))
						(s-decl
							(p-ident (raw "x_"))
							(e-int (raw "15")))
						(e-ident (raw "x_"))))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "redeclareTest"))
				(e-record)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "redeclareTest"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-var
					(p-assign (ident "x_"))
					(e-num (value "5")))
				(s-var
					(p-assign (ident "x_"))
					(e-num (value "10")))
				(s-reassign
					(p-assign (ident "x_"))
					(e-num (value "15")))
				(e-lookup-local
					(p-assign (ident "x_"))))))
	(d-let
		(p-assign (ident "result"))
		(e-call (constraint-fn-var 251)
			(e-lookup-local
				(p-assign (ident "redeclareTest")))
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "Dec")))
	(expressions
		(expr (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "Dec"))))
~~~
