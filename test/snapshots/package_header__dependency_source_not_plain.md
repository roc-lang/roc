# META
~~~ini
description=Package Header - dependency sources with interpolation or escapes
type=header
~~~
# SOURCE
~~~roc
package [Foo] { a: "../${x}/main.roc", b: "..\u(2F)b/main.roc" }
~~~
# EXPECTED
DEPENDENCY SOURCE NOT PLAIN - package_header__dependency_source_not_plain.md:1:20:1:38
DEPENDENCY SOURCE NOT PLAIN - package_header__dependency_source_not_plain.md:1:43:1:63
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Dependency Source Not Plain")
		(region (start 1 20) (end 1 38))
		(headline
			(reflow "I was parsing a dependency entry, and its path or URL uses string interpolation or an escape sequence."))
		(document
			(reflow "Dependency sources are read exactly as they are written, so write the path or URL out in full, without ")
			(annotated code "${...}")
			(reflow " or ")
			(annotated code "\\")
			(reflow " escapes.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "json: \"../json/main.roc\"")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "package_header__dependency_source_not_plain.md") (start 1 20) (end 1 38) (annotation error) (line-text "package [Foo] { a: \"../${x}/main.roc\", b: \"..\\u(2F)b/main.roc\" }"))))
	(report
		(severity runtime_error)
		(title "Dependency Source Not Plain")
		(region (start 1 43) (end 1 63))
		(headline
			(reflow "I was parsing a dependency entry, and its path or URL uses string interpolation or an escape sequence."))
		(document
			(reflow "Dependency sources are read exactly as they are written, so write the path or URL out in full, without ")
			(annotated code "${...}")
			(reflow " or ")
			(annotated code "\\")
			(reflow " escapes.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "json: \"../json/main.roc\"")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "package_header__dependency_source_not_plain.md") (start 1 43) (end 1 63) (annotation error) (line-text "package [Foo] { a: \"../${x}/main.roc\", b: \"..\\u(2F)b/main.roc\" }")))))
~~~
# TOKENS
~~~zig
KwPackage,OpenSquare,UpperIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(package
	(exposes
		(exposed-upper-ident (text "Foo")))
	(packages
		(record-field (name "a")
			(e-string
				(e-string-part (raw "../"))
				(e-ident (raw "x"))
				(e-string-part (raw "/main.roc"))))
		(record-field (name "b")
			(e-string
				(e-string-part (raw "..\u(2F)b/main.roc"))))))
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
