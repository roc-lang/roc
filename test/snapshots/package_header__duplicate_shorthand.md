# META
~~~ini
description=Package Header - package shorthand declared twice
type=header
~~~
# SOURCE
~~~roc
package [Foo] { dep: "../a/main.roc", dep: "../b/main.roc" }
~~~
# EXPECTED
DUPLICATE PACKAGE SHORTHAND - package_header__duplicate_shorthand.md:1:39:1:59
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Duplicate Package Shorthand")
		(region (start 1 39) (end 1 59))
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
			(source-region (file "package_header__duplicate_shorthand.md") (start 1 39) (end 1 59) (annotation error) (line-text "package [Foo] { dep: \"../a/main.roc\", dep: \"../b/main.roc\" }")))))
~~~
# TOKENS
~~~zig
KwPackage,OpenSquare,UpperIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(package
	(exposes
		(exposed-upper-ident (text "Foo")))
	(packages
		(record-field (name "dep")
			(e-string
				(e-string-part (raw "../a/main.roc"))))
		(record-field (name "dep")
			(e-string
				(e-string-part (raw "../b/main.roc"))))))
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
