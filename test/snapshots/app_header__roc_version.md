# META
~~~ini
description=App Header - pinned roc version
type=header
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../main.roc", roc: "nightly-2026-July-31-123c5d7" }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(app (roc-version "nightly-2026-July-31-123c5d7")
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
		(record-field (name "roc")
			(e-string
				(e-string-part (raw "nightly-2026-July-31-123c5d7"))))))
~~~
# FORMATTED
~~~roc
app [main!] {
	pf: platform "../main.roc",
	roc: "nightly-2026-July-31-123c5d7",
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
