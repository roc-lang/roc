# META
~~~ini
description=App Header - repeated roc version pin is reported once
type=header
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../main.roc", roc: "nightly-2026-08-05-24f0b47", roc: "nightly-2026-08-05-24f0b47" }
~~~
# EXPECTED
DUPLICATE ROC VERSION - app_header__duplicate_roc_version_reported_once.md:1:78:1:111
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Duplicate Roc Version")
		(region (start 1 78) (end 1 111))
		(headline
			(reflow "I was parsing a header, and it pins the `roc` version more than once."))
		(document
			(reflow "A header can pin at most one compiler version. Remove the extra ")
			(annotated code "roc")
			(reflow " entries.")
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
			(source-region (file "app_header__duplicate_roc_version_reported_once.md") (start 1 78) (end 1 111) (annotation error) (line-text "app [main!] { pf: platform \"../main.roc\", roc: \"nightly-2026-08-05-24f0b47\", roc: \"nightly-2026-08-05-24f0b47\" }")))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(app (roc-version "nightly-2026-08-05-24f0b47")
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
				(e-string-part (raw "nightly-2026-08-05-24f0b47"))))
		(record-field (name "roc")
			(e-string
				(e-string-part (raw "nightly-2026-08-05-24f0b47"))))))
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
