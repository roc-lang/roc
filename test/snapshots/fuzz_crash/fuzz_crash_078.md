# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
import#\
~~~
# EXPECTED
INCOMPLETE IMPORT - fuzz_crash_078.md:2:1:2:1
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Incomplete Import")
		(region (start 2 1) (end 2 1))
		(headline
			(reflow "I was parsing an import, and the mod path is incomplete."))
		(document
			(reflow "Imports must name a mod, optionally with a qualifier and exposing list.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "import Json/Decode exposing [decode]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_078.md") (start 2 1) (end 2 1) (annotation error) (line-text "")))))
~~~
# TOKENS
~~~zig
KwImport,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "incomplete_import"))))
~~~
# FORMATTED
~~~roc
# \
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
