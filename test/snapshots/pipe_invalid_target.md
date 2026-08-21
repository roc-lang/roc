# META
~~~ini
description=Pipe target diagnostic
type=expr
~~~
# SOURCE
~~~roc
1 |> 2
~~~
# EXPECTED
EXPECTED PIPE TARGET - pipe_invalid_target.md:1:6:1:7
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Pipe Target")
		(region (start 1 6) (end 1 7))
		(headline
			(reflow "I was parsing a pipe expression, and I expected a name or parenthesized expression after `|>`."))
		(document
			(reflow "The right side of a pipe must start with a value name, tag name, or parenthesized expression.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "value |> next")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "2")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "pipe_invalid_target.md") (start 1 6) (end 1 7) (annotation error) (line-text "1 |> 2")))))
~~~
# TOKENS
~~~zig
Int,OpPizza,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "expr_pipe_expects_ident"))
~~~
# FORMATTED
~~~roc

~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
