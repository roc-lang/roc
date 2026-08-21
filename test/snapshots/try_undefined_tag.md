# META
~~~ini
description=Try operator on undefined tag identifier
type=expr
canonicalize_diagnostics=true
~~~
# SOURCE
~~~roc
A?
~~~
# EXPECTED
TRY OPERATOR OUTSIDE FUNCTION - try_undefined_tag.md:1:1:1:3
TYPE MISMATCH - try_undefined_tag.md:1:1:1:2
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Try Operator Outside Function")
		(region (start 1 1) (end 1 3))
		(headline
			(reflow "The ")
			(annotated code "?")
			(reflow " operator can only be used inside function bodies because it can cause an early return."))
		(document
			(source-region (file "try_undefined_tag.md") (start 1 1) (end 1 3) (annotation error) (line-text "A?"))))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 1) (end 1 2))
		(headline
			(reflow "The")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "operator expects a")
			(reflow " ")
			(annotated code "Try")
			(reflow " ")
			(reflow "type (a tag union containing ONLY")
			(reflow " ")
			(annotated code "Ok")
			(reflow " ")
			(reflow "and")
			(reflow " ")
			(annotated code "Err")
			(reflow " ")
			(reflow "tags), but I found."))
		(document
			(source-region (file "try_undefined_tag.md") (start 1 1) (end 1 2) (annotation error) (line-text "A?"))
			(line-break)
			(reflow "This expression has type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[A, ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated underline "Tip:")
			(reflow " ")
			(reflow "Maybe wrap a value using")
			(reflow " ")
			(annotated code "Ok(value)")
			(reflow " ")
			(reflow "or")
			(reflow " ")
			(annotated code "Err(value)")
			(reflow "."))))
~~~
# TOKENS
~~~zig
UpperIdent,NoSpaceOpQuestion,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-question-suffix
	(e-tag (raw "A")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-tag (name "A")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-nominal-external (builtin)
							(p-applied-tag))))
				(value
					(e-lookup-local
						(p-assign (ident "#ok")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-nominal-external (builtin)
							(p-applied-tag))))
				(value
					(e-runtime-error (tag "return_outside_fn")))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
