# META
~~~ini
description=App Header - package shorthand declared twice
type=header
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../main.roc", dep: "../a/main.roc", dep: "../b/main.roc" }
~~~
# EXPECTED
DUPLICATE PACKAGE SHORTHAND - app_header__duplicate_shorthand.md:1:65:1:85
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Duplicate Package Shorthand")
		(region (start 1 65) (end 1 85))
		(headline
			(reflow "I was parsing a dependency record, and this name was already given to an earlier dependency in the same record."))
		(document
			(reflow "Each name in a dependency record refers to exactly one platform or package, so every entry needs its own name. Remove this entry, or rename one of the two.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ json: \"../json/main.roc\", json_old: \"../json_old/main.roc\" }")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "app_header__duplicate_shorthand.md") (start 1 65) (end 1 85) (annotation error) (line-text "app [main!] { pf: platform \"../main.roc\", dep: \"../a/main.roc\", dep: \"../b/main.roc\" }")))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(app
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
		(record-field (name "dep")
			(e-string
				(e-string-part (raw "../a/main.roc"))))
		(record-field (name "dep")
			(e-string
				(e-string-part (raw "../b/main.roc"))))))
~~~
# FORMATTED
~~~roc
app [main!] { dep: "../a/main.roc", dep: "../b/main.roc", pf: platform "../main.roc" }
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
