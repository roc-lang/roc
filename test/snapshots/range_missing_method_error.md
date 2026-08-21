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
MISSING METHOD - range_missing_method_error.md:1:5:1:14
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 1 5) (end 1 14))
		(headline
			(reflow "The value before this")
			(reflow " ")
			(annotated operator "..<")
			(reflow " ")
			(reflow "operator has a type that doesn't have a")
			(reflow " ")
			(annotated code "range_exclusive_to")
			(reflow " ")
			(reflow "method."))
		(document
			(source-region (file "range_missing_method_error.md") (start 1 5) (end 1 14) (annotation error) (line-text "r = \"a\"..<\"z\""))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "range_exclusive_to")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "The")
			(reflow " ")
			(annotated operator "..<")
			(reflow " ")
			(reflow "operator calls a method named")
			(reflow " ")
			(annotated code "range_exclusive_to")
			(reflow " ")
			(reflow "on the value preceding it, passing the value after the operator as the one argument."))))
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
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Range(Str)")))
	(expressions
		(expr (type "Range(Str)"))))
~~~
