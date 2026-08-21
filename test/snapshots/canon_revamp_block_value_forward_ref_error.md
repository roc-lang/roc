# META
~~~ini
description=Block-local value cannot be used before declaration
type=snippet
~~~
# SOURCE
~~~roc
x = {
    y + 1
    y = 5
}
~~~
# EXPECTED
NAME NOT IN SCOPE - canon_revamp_block_value_forward_ref_error.md:2:5:2:6
UNUSED VARIABLE - canon_revamp_block_value_forward_ref_error.md:3:5:3:6
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 2 5) (end 2 6))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "y")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "canon_revamp_block_value_forward_ref_error.md") (start 2 5) (end 2 6) (annotation error) (line-text "    y + 1"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 3 5) (end 3 6))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "y")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_y")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "canon_revamp_block_value_forward_ref_error.md") (start 3 5) (end 3 6) (annotation error) (line-text "    y = 5")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-block
				(statements
					(e-binop (op "+")
						(e-ident (raw "y"))
						(e-int (raw "1")))
					(s-decl
						(p-ident (raw "y"))
						(e-int (raw "5"))))))))
~~~
# FORMATTED
~~~roc
x = {
	y + 1
	y = 5
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-block
			(s-expr
				(e-runtime-error (tag "erroneous_value_expr")))
			(s-let
				(p-assign (ident "y"))
				(e-num (value "5")))
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{}")))
	(expressions
		(expr (type "{}"))))
~~~
