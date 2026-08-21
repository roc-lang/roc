# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import fS
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
INCOMPLETE IMPORT - fuzz_crash_046.md:1:20:1:26
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "ASCII Control Character")
		(headline
			(reflow "ASCII control characters are not allowed in Roc source code."))
		(document))
	(report
		(severity runtime_error)
		(title "Incomplete Import")
		(region (start 1 20) (end 1 26))
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
			(text "I found ")
			(annotated code "import")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_046.md") (start 1 20) (end 1 26) (annotation error) (line-text "app[]{f:platform\"\"}import f\u{14}S")))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,KwImport,LowerIdent,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides)
		(record-field (name "f")
			(e-string
				(e-string-part (raw ""))))
		(packages
			(record-field (name "f")
				(e-string
					(e-string-part (raw ""))))))
	(statements
		(s-malformed (tag "incomplete_import"))))
~~~
# FORMATTED
~~~roc
app [] { f: platform "" }
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
