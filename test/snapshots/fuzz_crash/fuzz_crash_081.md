# META
~~~ini
description=fuzz crash: 0.() incorrectly tokenized as float
type=snippet
~~~
# SOURCE
~~~roc
x = 0.()
~~~
# EXPECTED
EXPECTED RECORD ACCESSOR - fuzz_crash_081.md:1:6:1:7
UNRECOGNIZED SYNTAX - fuzz_crash_081.md:1:5:1:9
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Record Accessor")
		(region (start 1 6) (end 1 7))
		(headline
			(reflow "I was parsing access after `.`, and I expected a field name or tuple index."))
		(document
			(reflow "Required record access uses ")
			(annotated code ".name")
			(reflow ", optional record access uses ")
			(annotated code ".?name")
			(reflow ", and tuple access uses ")
			(annotated code ".0")
			(reflow ". Accessor names must be lowercase and adjacent to their punctuation.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "person.name")
			(line-break)
			(indent 1)
			(text "maybe_person.?name")
			(line-break)
			(indent 1)
			(text "pair.0")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_081.md") (start 1 6) (end 1 7) (annotation error) (line-text "x = 0.()"))))
	(report
		(severity runtime_error)
		(title "Unrecognized Syntax")
		(region (start 1 5) (end 1 9))
		(headline
			(reflow "I don't recognize this syntax."))
		(document
			(source-region (file "fuzz_crash_081.md") (start 1 5) (end 1 9) (annotation error) (line-text "x = 0.()"))
			(line-break)
			(reflow "This might be a syntax error, an unsupported language feature, or a typo."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,Dot,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-apply
				(e-malformed (reason "expr_dot_suffix_not_allowed"))))))
~~~
# FORMATTED
~~~roc
x = ()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
