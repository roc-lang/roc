# META
~~~ini
description=Variable scoping with var keyword
type=snippet
~~~
# SOURCE
~~~roc
# This should cause an error - var not allowed at top level
var topLevelVar_ = 0
~~~
# EXPECTED
VAR OUTSIDE BODY - can_var_scoping_invalid_top_level.md:2:1:2:4
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Var Outside Body")
		(region (start 2 1) (end 2 4))
		(headline
			(reflow "I was parsing a statement, and `var` appeared outside a function or block body."))
		(document
			(reflow "Mutable variables are local body statements. Move this ")
			(annotated code "var")
			(reflow " into a body, or use an ordinary top-level declaration.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "main = {")
			(line-break)
			(indent 1)
			(text "    var count = 0")
			(line-break)
			(indent 1)
			(text "    count")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "var")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "can_var_scoping_invalid_top_level.md") (start 2 1) (end 2 4) (annotation error) (line-text "var topLevelVar_ = 0")))))
~~~
# TOKENS
~~~zig
KwVar,LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "var_only_allowed_in_a_body"))
		(s-decl
			(p-ident (raw "topLevelVar_"))
			(e-int (raw "0")))))
~~~
# FORMATTED
~~~roc
# This should cause an error - var not allowed at top level
topLevelVar_ = 0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "topLevelVar_"))
		(e-num (value "0"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec")))
	(expressions
		(expr (type "Dec"))))
~~~
