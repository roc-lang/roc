# META
~~~ini
description=Package Header - pinned roc version
type=header
~~~
# SOURCE
~~~roc
package [Foo] { roc: "0.1.0", other: "../../other/main.roc" }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPackage,OpenSquare,UpperIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(package (roc-version "0.1.0")
	(exposes
		(exposed-upper-ident (text "Foo")))
	(packages
		(record-field (name "roc")
			(e-string
				(e-string-part (raw "0.1.0"))))
		(record-field (name "other")
			(e-string
				(e-string-part (raw "../../other/main.roc"))))))
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
