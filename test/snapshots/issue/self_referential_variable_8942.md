# META
~~~ini
description=Regression test for stack overflow with self-referential variable (issue #8942)
type=snippet
~~~
# SOURCE
~~~roc
a = a
~~~
# EXPECTED
INVALID ASSIGNMENT TO ITSELF - self_referential_variable_8942.md:1:5:1:6
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Assignment To Itself")
		(region (start 1 5) (end 1 6))
		(headline
			(reflow "The value ")
			(annotated symbol-unqualified "a")
			(reflow " is assigned to itself, which would cause an infinite loop at runtime."))
		(document
			(reflow "Only functions can reference themselves (for recursion). For non-function values, the right-hand side must be fully computable without referring to the value being assigned.")
			(line-break)
			(line-break)
			(source-region (file "self_referential_variable_8942.md") (start 1 5) (end 1 6) (annotation error) (line-text "a = a")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-ident (raw "a")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-runtime-error (tag "self_referential_definition"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
