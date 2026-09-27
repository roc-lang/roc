# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
me = "luc"
foo = "hello ${namF
~~~
# EXPECTED
EXPECTED INTERPOLATION END - fuzz_crash_017.md:2:7:2:8
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Interpolation End")
		(region (start 2 7) (end 2 8))
		(headline
			(reflow "I was parsing a string interpolation, and I expected `}` before returning to the string."))
		(document
			(reflow "String interpolations start with ")
			(annotated code "${")
			(reflow " and must close with ")
			(annotated code "}")
			(reflow " after the embedded expression.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "\"Hello, ${name}!\"")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "\"")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_017.md") (start 2 7) (end 2 8) (annotation error) (line-text "foo = \"hello ${namF")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "me"))
			(e-string
				(e-string-part (raw "luc"))))
		(s-decl
			(p-ident (raw "foo"))
			(e-malformed (reason "string_expected_close_interpolation")))))
~~~
# FORMATTED
~~~roc
me = "luc"

foo =
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "me"))
		(e-string
			(e-literal (string "luc"))))
	(d-let
		(p-assign (ident "foo"))
		(e-runtime-error (tag "expr_syntax_error"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str"))
		(patt (type "Error")))
	(expressions
		(expr (type "Str"))
		(expr (type "Error"))))
~~~
