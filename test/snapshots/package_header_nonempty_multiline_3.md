# META
~~~ini
description=package_header_nonempty_multiline (3)
type=file
~~~
# SOURCE
~~~roc
package
	[something, SomeType,]
	{ somePkg: "../main.roc", }
~~~
# EXPECTED
MOD NOT FOUND - package_header_nonempty_multiline_3.md:2:14:2:22
EXPOSED BUT NOT DEFINED - package_header_nonempty_multiline_3.md:2:3:2:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 2 14) (end 2 22))
		(headline
			(text "The mod ")
			(annotated code "SomeType")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "package_header_nonempty_multiline_3.md") (start 2 14) (end 2 22) (annotation error) (line-text "\t[something, SomeType,]"))))
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 2 3) (end 2 12))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "something")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "package_header_nonempty_multiline_3.md") (start 2 3) (end 2 12) (annotation error) (line-text "\t[something, SomeType,]"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "something")
			(reflow " in this mod, or by removing it from the list of exposed values."))))
~~~
# TOKENS
~~~zig
KwPackage,
OpenSquare,LowerIdent,Comma,UpperIdent,Comma,CloseSquare,
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,CloseCurly,
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
package
	[
		something,
		SomeType,
	]
	{
		somePkg: "../main.roc",
	}
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
