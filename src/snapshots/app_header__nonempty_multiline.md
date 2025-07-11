# META
~~~ini
description=App Header = nonempty multiline
type=header
~~~
# SOURCE
~~~roc
app # This comment is here
	[main!]
	{ pf: platform "../main.roc", somePkg: "../main.roc" }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),Newline(1:6-1:27),
OpenSquare(2:2-2:3),LowerIdent(2:3-2:8),CloseSquare(2:8-2:9),Newline(1:1-1:1),
OpenCurly(3:2-3:3),LowerIdent(3:4-3:6),OpColon(3:6-3:7),KwPlatform(3:8-3:16),StringStart(3:17-3:18),StringPart(3:18-3:29),StringEnd(3:29-3:30),Comma(3:30-3:31),LowerIdent(3:32-3:39),OpColon(3:39-3:40),StringStart(3:41-3:42),StringPart(3:42-3:53),StringEnd(3:53-3:54),CloseCurly(3:55-3:56),EndOfFile(3:56-3:56),
~~~
# PARSE
~~~clojure
(app @1.1-3.56
	(provides @2.3-2.9
		(exposed-lower-ident (text "main!")))
	(record-field @3.4-3.31 (name "pf")
		(e-string @3.17-3.30
			(e-string-part @3.18-3.29 (raw "../main.roc"))))
	(packages @3.2-3.56
		(record-field @3.4-3.31 (name "pf")
			(e-string @3.17-3.30
				(e-string-part @3.18-3.29 (raw "../main.roc"))))
		(record-field @3.32-3.56 (name "somePkg")
			(e-string @3.41-3.54
				(e-string-part @3.42-3.53 (raw "../main.roc"))))))
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
