# META
~~~ini
description=A top-level destructure whose right-hand side is a block returning one of its own bound names reads its own unfinished binding, which is an invalid self-reference rather than a compiler stack overflow
type=file
~~~
# SOURCE
~~~roc
{x}={x}
~~~
# EXPECTED
INVALID ASSIGNMENT TO ITSELF - top_level_destructure_value_cycle.md:1:6:1:7
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Assignment To Itself")
		(region (start 1 6) (end 1 7))
		(headline
			(reflow "The value ")
			(annotated symbol-unqualified "x")
			(reflow " is assigned to itself, which would cause an infinite loop at runtime."))
		(document
			(reflow "Only functions can reference themselves (for recursion). For non-function values, the right-hand side must be fully computable without referring to the value being assigned.")
			(line-break)
			(line-break)
			(source-region (file "top_level_destructure_value_cycle.md") (start 1 6) (end 1 7) (annotation error) (line-text "{x}={x}")))))
~~~
# TOKENS
~~~zig
OpenCurly,LowerIdent,CloseCurly,OpAssign,OpenCurly,LowerIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-record
				(field (name "x") (rest false)))
			(e-block
				(statements
					(e-ident (raw "x")))))))
~~~
# FORMATTED
~~~roc
{ x } = {
	x
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-record-destructure
			(destructs
				(record-destruct (label "x") (ident "x")
					(required
						(p-assign (ident "x"))))))
		(e-block
			(e-runtime-error (tag "self_referential_definition")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions
		(expr (type "{ x: _field }"))))
~~~
