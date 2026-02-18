# META
~~~ini
description=App Header - nonempty multiline w/ trailing comma
type=header
~~~
# SOURCE
~~~roc
app
	[main!,]
	{ pf: platform "../main.roc", somePkg: "../main.roc", }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,
OpenSquare,LowerIdent,Comma,CloseSquare,
OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,CloseCurly,
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
		(record-field (name "somePkg")
			(e-string
				(e-string-part (raw "../main.roc"))))))
~~~
# FORMATTED
~~~roc
app
	[
		main!,
	]
	{
		pf: platform "../main.roc",
		somePkg: "../main.roc",
	}
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
