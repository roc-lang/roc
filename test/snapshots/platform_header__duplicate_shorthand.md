# META
~~~ini
description=Platform Header - package shorthand declared twice
type=header
~~~
# SOURCE
~~~roc
platform "test-platform"
	requires {}
	exposes []
	packages { dep: "../a/main.roc", dep: "../b/main.roc" }
	provides {}
~~~
# EXPECTED
DUPLICATE PACKAGE SHORTHAND - platform_header__duplicate_shorthand.md:4:35:4:55
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Duplicate Package Shorthand")
		(region (start 4 35) (end 4 55))
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
			(source-region (file "platform_header__duplicate_shorthand.md") (start 4 35) (end 4 55) (annotation error) (line-text "\tpackages { dep: \"../a/main.roc\", dep: \"../b/main.roc\" }")))))
~~~
# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,CloseCurly,
KwExposes,OpenSquare,CloseSquare,
KwPackages,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
KwProvides,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(platform (name "test-platform")
	(requires)
	(exposes)
	(packages
		(record-field (name "dep")
			(e-string
				(e-string-part (raw "../a/main.roc"))))
		(record-field (name "dep")
			(e-string
				(e-string-part (raw "../b/main.roc")))))
	(provides))
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
