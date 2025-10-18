# META
~~~ini
description=App Header - nonempty multiline - heavily commented
type=header
~~~
# SOURCE
~~~roc
app # Comment after keyword
	[ # Comment after provides open
		main!, # Comment after exposed item
	]
	{ # Comment after packages open
		pf: platform "../main.roc", # Comment after platform
		other: "../../other/main.roc", # Comment after last package
	}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,
OpenSquare,
LowerIdent,Comma,
CloseSquare,
OpenCurly,
LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(app
	(provides
		(exposed-lower-ident
			(text "main!")))
	(record-field (name "pf")
		(e-string
			(e-string-part (raw "../main.roc"))))
	(packages
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../main.roc"))))
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
