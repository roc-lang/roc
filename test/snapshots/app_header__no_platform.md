# META
~~~ini
description=App Header - no platform entry gets the default platform
type=header
~~~
# SOURCE
~~~roc
app [main!] { unicode: "https://example.com/unicode.tar.zst" }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(app
	(provides
		(exposed-lower-ident
			(text "main!")))
	(packages
		(record-field (name "unicode")
			(e-string
				(e-string-part (raw "https://example.com/unicode.tar.zst"))))))
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
