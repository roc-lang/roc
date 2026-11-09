# META
~~~ini
description=Package Header - dependency values that are not strings
type=header
~~~
# SOURCE
~~~roc
package [Foo] { a: 42, b, c: "../c/main.roc" }
~~~
# EXPECTED
EXPECTED PACKAGE PATH - package_header__dependency_not_a_string.md:1:20:1:22
EXPECTED DEPENDENCY COLON - package_header__dependency_not_a_string.md:1:25:1:26
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Package Path")
		(region (start 1 20) (end 1 22))
		(headline
			(reflow "I was parsing a dependency entry, and I expected a string path."))
		(document
			(reflow "Normal package dependencies use a string path after the colon. Use ")
			(annotated code "platform \"...\"")
			(reflow " only for the single platform entry.")
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
			(text "I found ")
			(annotated code "42")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "package_header__dependency_not_a_string.md") (start 1 20) (end 1 22) (annotation error) (line-text "package [Foo] { a: 42, b, c: \"../c/main.roc\" }"))))
	(report
		(severity runtime_error)
		(title "Expected Dependency Colon")
		(region (start 1 25) (end 1 26))
		(headline
			(reflow "I was parsing a dependency entry, and I expected `:` after the field name."))
		(document
			(reflow "A dependency entry uses a colon between the local package name and the package path.")
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
			(text "I found ")
			(annotated code ",")
			(text " here.")
			(line-break)
			(reflow "A comma separates items, but there must be a valid item on both sides of it.")
			(line-break)
			(line-break)
			(source-region (file "package_header__dependency_not_a_string.md") (start 1 25) (end 1 26) (annotation error) (line-text "package [Foo] { a: 42, b, c: \"../c/main.roc\" }")))))
~~~
# TOKENS
~~~zig
KwPackage,OpenSquare,UpperIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(package
	(exposes
		(exposed-upper-ident (text "Foo")))
	(packages
		(record-field (name "a")
			(e-int (raw "42")))
		(record-field (name "b"))
		(record-field (name "c")
			(e-string
				(e-string-part (raw "../c/main.roc"))))))
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
