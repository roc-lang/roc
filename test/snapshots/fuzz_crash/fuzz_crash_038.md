# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
*import B as
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_038.md:1:1:1:2
EXPECTED IMPORT ALIAS - fuzz_crash_038.md:1:2:1:8
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 1) (end 1 2))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "*")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_038.md") (start 1 1) (end 1 2) (annotation error) (line-text "*import B as"))))
	(report
		(severity runtime_error)
		(title "Expected Import Alias")
		(region (start 1 2) (end 1 8))
		(headline
			(reflow "I was parsing an import alias, and I expected an uppercase name after `as`."))
		(document
			(reflow "Import aliases must start with an uppercase letter.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "import Json/Decode as Decode")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "import")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_038.md") (start 1 2) (end 1 8) (annotation error) (line-text "*import B as")))))
~~~
# TOKENS
~~~zig
OpStar,KwImport,UpperIdent,KwAs,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_upper_name_after_import_as"))))
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
