# META
~~~ini
description=App Header - roc version that is not a version
type=header
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../main.roc", roc: "yesterday's build" }
~~~
# EXPECTED
INVALID ROC VERSION - app_header__roc_version_invalid.md:1:43:1:67
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Roc Version")
		(region (start 1 43) (end 1 67))
		(headline
			(reflow "I was parsing the `roc` entry of a header, and I did not recognize this version."))
		(document
			(reflow "The ")
			(annotated code "roc")
			(reflow " entry pins the version of the Roc compiler this file is written for. It must be a string holding either a nightly tag or a release version.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "roc: \"nightly-2026-08-05-24f0b47\"")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "app_header__roc_version_invalid.md") (start 1 43) (end 1 67) (annotation error) (line-text "app [main!] { pf: platform \"../main.roc\", roc: \"yesterday's build\" }")))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(app (roc-version "yesterday's build")
	(provides
		(exposed-lower-ident
			(text "main!")))
	(record-field (name "pf")
		(e-string
			(e-string-part (raw "../main.roc"))))
	(packages
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../main.roc"))))
		(record-field (name "roc")
			(e-string
				(e-string-part (raw "yesterday's build"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
