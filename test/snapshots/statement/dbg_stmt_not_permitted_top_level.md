# META
~~~ini
description=Debug expression not permitted at the top level
type=snippet
~~~
# SOURCE
~~~roc
# not permitted
dbg "foo"

foo = ...
~~~
# EXPECTED
INVALID STATEMENT - dbg_stmt_not_permitted_top_level.md:2:1:2:10
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Statement")
		(region (start 2 1) (end 2 10))
		(headline
			(reflow "The statement ")
			(annotated code "dbg")
			(reflow " is not allowed at the top level."))
		(document
			(reflow "Only definitions, type annotations, and imports are allowed at the top level.")
			(line-break)
			(line-break)
			(source-region (file "dbg_stmt_not_permitted_top_level.md") (start 2 1) (end 2 10) (annotation error) (line-text "dbg \"foo\"")))))
~~~
# TOKENS
~~~zig
KwDbg,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,TripleDot,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-dbg
			(e-string
				(e-string-part (raw "foo"))))
		(s-decl
			(p-ident (raw "foo"))
			(e-ellipsis))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-not-implemented)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{}")))
	(expressions
		(expr (type "{}"))))
~~~
