# META
~~~ini
description=A top-level destructure whose right-hand side is a block returning one of its own bound names is a circular value definition, not a compiler stack overflow
type=file
~~~
# SOURCE
~~~roc
{x}={x}
~~~
# EXPECTED
CIRCULAR VALUE DEFINITION - top_level_destructure_value_cycle.md:1:2:1:3
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Circular Value Definition")
		(region (start 1 2) (end 1 3))
		(headline
			(reflow "The value ")
			(annotated symbol-unqualified "x")
			(reflow " is part of a recursive non-function definition cycle."))
		(document
			(reflow "Only functions can be recursive. Non-function top-level values must be fully computable without depending on themselves through other values.")
			(line-break)
			(line-break)
			(source-region (file "top_level_destructure_value_cycle.md") (start 1 2) (end 1 3) (annotation error) (line-text "{x}={x}")))))
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
		(e-runtime-error (tag "circular_value_definition"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions
		(expr (type "Error"))))
~~~
