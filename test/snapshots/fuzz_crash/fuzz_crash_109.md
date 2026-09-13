# META
~~~ini
description=parser formatter stability: qualified wildcard package exposure
type=file
~~~
# SOURCE
~~~roc
package[e,E.a.*]{}
~~~
# EXPECTED
MOD NOT FOUND - fuzz_crash_109.md:1:11:1:16
EXPOSED BUT NOT DEFINED - fuzz_crash_109.md:1:9:1:10
EXPOSED BUT NOT DEFINED - fuzz_crash_109.md:1:11:1:16
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 1 11) (end 1 16))
		(headline
			(text "The mod ")
			(annotated code "E")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "fuzz_crash_109.md") (start 1 11) (end 1 16) (annotation error) (line-text "package[e,E.a.*]{}"))))
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 1 9) (end 1 10))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "e")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "fuzz_crash_109.md") (start 1 9) (end 1 10) (annotation error) (line-text "package[e,E.a.*]{}"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "e")
			(reflow " in this mod, or by removing it from the list of exposed values.")))
	(report
		(severity runtime_error)
		(title "Exposed But Not Defined")
		(region (start 1 11) (end 1 16))
		(headline
			(reflow "The mod header says that ")
			(annotated symbol-unqualified "E.a")
			(reflow " is exposed, but it is not defined anywhere in this mod."))
		(document
			(source-region (file "fuzz_crash_109.md") (start 1 11) (end 1 16) (annotation error) (line-text "package[e,E.a.*]{}"))
			(reflow "You can fix this by either defining ")
			(annotated symbol-unqualified "E.a")
			(reflow " in this mod, or by removing it from the list of exposed values."))))
~~~
# TOKENS
~~~zig
KwPackage,OpenSquare,LowerIdent,Comma,UpperIdent,NoSpaceDotLowerIdent,DotStar,CloseSquare,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(package
		(exposes
			(exposed-lower-ident
				(text "e"))
			(exposed-upper-ident-star (text "E.a")))
		(packages))
	(statements))
~~~
# FORMATTED
~~~roc
package [e, E.a.*] {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (mod "E")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
