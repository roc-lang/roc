# META
~~~ini
description=package_header_nonempty_singleline (1)
type=file
~~~
# SOURCE
~~~roc
package [something, SomeType] { somePkg: "../main.roc", other: "../../other/main.roc" }
~~~
# EXPECTED
MOD NOT FOUND - package_header_nonempty_singleline_1.md:1:21:1:29
EXPOSED BUT NOT DEFINED - package_header_nonempty_singleline_1.md:1:10:1:19
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 1 21) (end 1 29))
		(headline
			(text "The mod ")
			(annotated code "SomeType")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "package_header_nonempty_singleline_1.md") (start 1 21) (end 1 29) (annotation error) (line-text "package [something, SomeType] { somePkg: \"../main.roc\", other: \"../../other/main.roc\" }"))))
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 1 10) (end 1 19))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "something")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "package_header_nonempty_singleline_1.md") (start 1 10) (end 1 19) (annotation error) (line-text "package [something, SomeType] { somePkg: \"../main.roc\", other: \"../../other/main.roc\" }"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "something")
			(reflow " in this mod, or by removing it from the list of exposed values."))))
~~~
# TOKENS
~~~zig
KwPackage,OpenSquare,LowerIdent,Comma,UpperIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(package
		(exposes
			(exposed-lower-ident
				(text "something"))
			(exposed-upper-ident (text "SomeType")))
		(packages
			(record-field (name "somePkg")
				(e-string
					(e-string-part (raw "../main.roc"))))
			(record-field (name "other")
				(e-string
					(e-string-part (raw "../../other/main.roc"))))))
	(statements))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (mod "SomeType")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
