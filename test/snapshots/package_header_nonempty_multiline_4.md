# META
~~~ini
description=package_header_nonempty_multiline (4)
type=file
~~~
# SOURCE
~~~roc
package
	[
		something,
		SomeType,
	]
	{
		somePkg: "../main.roc",
	}
~~~
# EXPECTED
MOD NOT FOUND - package_header_nonempty_multiline_4.md:4:3:4:11
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_4.md:3:3:3:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 4 3) (end 4 11))
		(headline
			(text "The mod ")
			(annotated code "SomeType")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "package_header_nonempty_multiline_4.md") (start 4 3) (end 4 11) (annotation error) (line-text "\t\tSomeType,"))))
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 3 3) (end 3 12))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "something")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "package_header_nonempty_multiline_4.md") (start 3 3) (end 3 12) (annotation error) (line-text "\t\tsomething,"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "something")
			(reflow " in this mod, or by removing it from the list of exposed values."))))
~~~
# TOKENS
~~~zig
KwPackage,
OpenSquare,
LowerIdent,Comma,
UpperIdent,Comma,
CloseSquare,
OpenCurly,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
CloseCurly,
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
					(e-string-part (raw "../main.roc"))))))
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
