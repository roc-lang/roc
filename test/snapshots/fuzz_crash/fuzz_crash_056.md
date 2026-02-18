# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform"",r:"",o:""}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides)
		(record-field (name "f")
			(e-string
				(e-string-part (raw ""))))
		(packages
			(record-field (name "f")
				(e-string
					(e-string-part (raw ""))))
			(record-field (name "r")
				(e-string
					(e-string-part (raw ""))))
			(record-field (name "o")
				(e-string
					(e-string-part (raw ""))))))
	(statements))
~~~
# FORMATTED
~~~roc
app [] { f: platform "", r: "", o: "" }
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
