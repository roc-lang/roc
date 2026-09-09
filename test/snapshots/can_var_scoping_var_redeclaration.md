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
VAR NAME MISSING `$` - can_var_scoping_var_redeclaration.md:3:6:3:8
DUPLICATE DEFINITION - can_var_scoping_var_redeclaration.md:4:6:4:8
VAR NAME MISSING `$` - can_var_scoping_var_redeclaration.md:4:6:4:8
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Var Name Missing `$`")
		(region (start 3 6) (end 3 8))
		(headline
			(reflow "The mutable binding ")
			(annotated symbol-unqualified "x_")
			(reflow " is declared with ")
			(annotated keyword "var")
			(reflow " but its name does not start with ")
			(annotated code "$")
			(reflow "."))
		(document
			(reflow "Rename this binding and all of its uses to ")
			(annotated symbol-unqualified "$x_")
			(reflow ". The name is only a convention; mutability comes from the ")
			(annotated keyword "var")
			(reflow " declaration.")
			(line-break)
			(line-break)
			(source-region (file "can_var_scoping_var_redeclaration.md") (start 3 6) (end 3 8) (annotation warning) (line-text "\tvar x_ = 5"))))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 4 6) (end 4 8))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "x_")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "can_var_scoping_var_redeclaration.md") (start 4 6) (end 4 8) (annotation error) (line-text "\tvar x_ = 10 # Redeclare var - should warn but proceed"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "x_")
			(reflow " was already defined in ")
			(source-location
				(file "can_var_scoping_var_redeclaration.md")
				(line 3)
				(column 6))
			(reflow ":")
			(line-break)
			(source-region (file "can_var_scoping_var_redeclaration.md") (start 3 6) (end 3 8) (annotation dim) (line-text "\tvar x_ = 5"))))
	(report
		(severity warning)
		(title "Var Name Missing `$`")
		(region (start 4 6) (end 4 8))
		(headline
			(reflow "The mutable binding ")
			(annotated symbol-unqualified "x_")
			(reflow " is declared with ")
			(annotated keyword "var")
			(reflow " but its name does not start with ")
			(annotated code "$")
			(reflow "."))
		(document
			(reflow "Rename this binding and all of its uses to ")
			(annotated symbol-unqualified "$x_")
			(reflow ". The name is only a convention; mutability comes from the ")
			(annotated keyword "var")
			(reflow " declaration.")
			(line-break)
			(line-break)
			(source-region (file "can_var_scoping_var_redeclaration.md") (start 4 6) (end 4 8) (annotation warning) (line-text "\tvar x_ = 10 # Redeclare var - should warn but proceed")))))
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
					(p-var-assign (ident "x_"))
					(e-num (value "5")))
				(s-var
					(p-var-assign (ident "x_"))
					(e-num (value "10")))
				(s-reassign
					(p-var-assign (ident "x_"))
					(e-num (value "15")))
				(e-lookup-local
					(p-var-assign (ident "x_"))))))
	(d-let
		(p-assign (ident "result"))
		(e-call (constraint-fn-var 253)
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
