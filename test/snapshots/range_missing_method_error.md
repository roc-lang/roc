# META
~~~ini
description=Range over non-numeric type reports a missing range_exclusive method
type=snippet
~~~
# SOURCE
~~~roc
r = "a"..<"z"
~~~
# EXPECTED
TYPE NOT DETERMINED - range_missing_method_error.md:1:5:1:8
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Not Determined")
		(region (start 1 5) (end 1 8))
		(headline
			(reflow "Nothing in this program determines the type of this string:"))
		(document
			(source-region (file "range_missing_method_error.md") (start 1 5) (end 1 8) (annotation error) (line-text "r = \"a\"..<\"z\""))
			(line-break)
			(reflow "Its type needs all of these:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "num where [num.range_exclusive_to : num, num -> Range(num)]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Without knowing which type it is, there's no way to tell which")
			(reflow " ")
			(annotated operator "..<")
			(reflow " ")
			(reflow "to use.")
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "None of the built-in string types")
			(reflow " ")
			(reflow "support")
			(reflow " ")
			(annotated operator "..<")
			(reflow "."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,OpDoubleDotLessThan,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "r"))
			(e-binop (op "..<")
				(e-string
					(e-string-part (raw "a")))
				(e-string
					(e-string-part (raw "z")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "r"))
		(e-dispatch-call (method "range_exclusive_to") (constraint-fn-var 232)
			(receiver
				(e-runtime-error (tag "erroneous_value_expr")))
			(args
				(e-runtime-error (tag "erroneous_value_expr"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Range(Str)")))
	(expressions
		(expr (type "Range(Str)"))))
~~~
