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
KwApp(1:1-1:4),
OpenSquare(2:2-2:3),
LowerIdent(3:3-3:8),Comma(3:8-3:9),
CloseSquare(4:2-4:3),
OpenCurly(5:2-5:3),
LowerIdent(6:3-6:5),OpColon(6:5-6:6),KwPlatform(6:7-6:15),StringStart(6:16-6:17),StringPart(6:17-6:28),StringEnd(6:28-6:29),Comma(6:29-6:30),
LowerIdent(7:3-7:8),OpColon(7:8-7:9),StringStart(7:10-7:11),StringPart(7:11-7:31),StringEnd(7:31-7:32),Comma(7:32-7:33),
CloseCurly(8:2-8:3),EndOfFile(8:3-8:3),
~~~
# PARSE
~~~clojure
(app @1.1-8.3
	(provides @2.2-4.3
		(exposed-lower-ident @3.3-3.8
			(text "main!")))
	(record-field @6.3-6.29 (name "pf")
		(e-string @6.16-6.29
			(e-string-part @6.17-6.28 (raw "../main.roc"))))
	(packages @5.2-8.3
		(record-field @6.3-6.29 (name "pf")
			(e-string @6.16-6.29
				(e-string-part @6.17-6.28 (raw "../main.roc"))))
		(record-field @7.3-7.32 (name "other")
			(e-string @7.10-7.32
				(e-string-part @7.11-7.31 (raw "../../other/main.roc"))))))
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
