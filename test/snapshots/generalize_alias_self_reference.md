# META
~~~ini
description=A self-referential value binding (x = x) should be a diagnostic, not a crash
type=snippet
~~~
# SOURCE
~~~roc
x = x

main = x
~~~
# EXPECTED
INVALID ASSIGNMENT TO ITSELF - generalize_alias_self_reference.md:1:5:1:6
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Assignment To Itself")
		(region (start 1 5) (end 1 6))
		(headline
			(reflow "The value ")
			(annotated symbol-unqualified "x")
			(reflow " is assigned to itself, which would cause an infinite loop at runtime."))
		(document
			(reflow "Only functions can reference themselves (for recursion). For non-function values, the right-hand side must be fully computable without referring to the value being assigned.")
			(line-break)
			(line-break)
			(source-region (file "generalize_alias_self_reference.md") (start 1 5) (end 1 6) (annotation error) (line-text "x = x")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-ident (raw "x")))
		(s-decl
			(p-ident (raw "main"))
			(e-ident (raw "x")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-runtime-error (tag "self_referential_definition")))
	(d-let
		(p-assign (ident "main"))
		(e-runtime-error (tag "erroneous_value_use"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
