# META
~~~ini
description=Parser formatter round-trip failure on carriage return byte
type=file
source_escapes=true
~~~
# SOURCE
~~~roc
a=(0\r.e)
~~~
# EXPECTED
MISPLACED CARRIAGE RETURN - :0:0:0:0
MISSING METHOD - fuzz_crash_098.md:1:4:1:5
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Misplaced Carriage Return")
		(headline
			(reflow "Carriage return characters (\\r) are not allowed in Roc source code."))
		(document))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 1 4) (end 1 5))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_numeral")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "fuzz_crash_098.md") (start 1 4) (end 1 5) (annotation error) (line-text "a=(0\r.e)"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "from_numeral")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ e: _field, .. }")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,NoSpaceOpenRound,Int,DotLowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-tuple
				(e-field-access
					(receiver
						(e-int (raw "0")))
					(segment (mode "required") (field "e")))))))
~~~
# FORMATTED
~~~roc
a = ((0).e)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "erroneous_value_expr")))
			(segments
				(segment (name "e") (mode "required"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_b")))
	(expressions
		(expr (type "_b"))))
~~~
