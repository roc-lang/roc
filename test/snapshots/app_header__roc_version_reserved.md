# META
~~~ini
description=App Header - roc used as a platform shorthand
type=header
~~~
# SOURCE
~~~roc
app [main!] { roc: platform "../main.roc" }
~~~
# EXPECTED
RESERVED DEPENDENCY NAME - app_header__roc_version_reserved.md:1:15:1:42
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Reserved Dependency Name")
		(region (start 1 15) (end 1 42))
		(headline
			(reflow "I was parsing a dependency record, and `roc` is used as the name of a platform or package."))
		(document
			(reflow "The ")
			(annotated code "roc")
			(reflow " name is reserved for pinning the compiler version, so it cannot name a dependency. Pick a different name for this one.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "pf: platform \"../platform/main.roc\"")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "app_header__roc_version_reserved.md") (start 1 15) (end 1 42) (annotation error) (line-text "app [main!] { roc: platform \"../main.roc\" }")))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(app
	(provides
		(exposed-lower-ident
			(text "main!")))
	(record-field (name "roc")
		(e-string
			(e-string-part (raw "../main.roc"))))
	(packages
		(record-field (name "roc")
			(e-string
				(e-string-part (raw "../main.roc"))))))
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
