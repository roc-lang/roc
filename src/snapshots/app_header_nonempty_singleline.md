# META
~~~ini
description=App Header - nonempty singleline
type=header
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../main.roc", other: "../../other/main.roc" }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:40),StringEnd(1:40-1:41),Comma(1:41-1:42),LowerIdent(1:43-1:48),OpColon(1:48-1:49),StringStart(1:50-1:51),StringPart(1:51-1:71),StringEnd(1:71-1:72),CloseCurly(1:73-1:74),EndOfFile(1:74-1:74),
~~~
# PARSE
~~~clojure
(app @1.1-1.74
	(provides @1.6-1.12
		(exposed-lower-ident (text "main!")))
	(record-field @1.15-1.42 (name "pf")
		(e-string @1.28-1.41
			(e-string-part @1.29-1.40 (raw "../main.roc"))))
	(packages @1.13-1.74
		(record-field @1.15-1.42 (name "pf")
			(e-string @1.28-1.41
				(e-string-part @1.29-1.40 (raw "../main.roc"))))
		(record-field @1.43-1.74 (name "other")
			(e-string @1.50-1.72
				(e-string-part @1.51-1.71 (raw "../../other/main.roc"))))))
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
