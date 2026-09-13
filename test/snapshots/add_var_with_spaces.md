# META
~~~ini
description=Add a variable with spaces
type=snippet
~~~
# SOURCE
~~~roc
add2 = x +      2
~~~
# EXPECTED
NAME NOT IN SCOPE - add_var_with_spaces.md:1:8:1:9
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 1 8) (end 1 9))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "x")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "add_var_with_spaces.md") (start 1 8) (end 1 9) (annotation error) (line-text "add2 = x +      2")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "add2"))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-int (raw "2"))))))
~~~
# FORMATTED
~~~roc
add2 = x + 2
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "add2"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
