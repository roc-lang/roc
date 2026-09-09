# META
~~~ini
description=main mod from package
type=package
~~~
# SOURCE
~~~roc
package [
    Color,
] {}
~~~
# EXPECTED
MOD NOT FOUND - main.md:2:5:2:10
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 2 5) (end 2 10))
		(headline
			(text "The mod ")
			(annotated code "Color")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "main.md") (start 2 5) (end 2 10) (annotation error) (line-text "    Color,")))))
~~~
# TOKENS
~~~zig
KwPackage,OpenSquare,
UpperIdent,Comma,
CloseSquare,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(package
		(exposes
			(exposed-upper-ident (text "Color")))
		(packages))
	(statements))
~~~
# FORMATTED
~~~roc
package
	[
		Color,
	]
	{}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (mod "Color")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
