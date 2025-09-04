# META
~~~ini
description=Singleline formatting app
type=file
~~~
# SOURCE
~~~roc
app [a1!, a2!] { pf: platform "../basic-cli/main.roc", a: "a" }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:9),Comma(1:9-1:10),LowerIdent(1:11-1:14),CloseSquare(1:14-1:15),OpenCurly(1:16-1:17),LowerIdent(1:18-1:20),OpColon(1:20-1:21),KwPlatform(1:22-1:30),StringStart(1:31-1:32),StringPart(1:32-1:53),StringEnd(1:53-1:54),Comma(1:54-1:55),LowerIdent(1:56-1:57),OpColon(1:57-1:58),StringStart(1:59-1:60),StringPart(1:60-1:61),StringEnd(1:61-1:62),CloseCurly(1:63-1:64),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.64
	(app @1.1-1.64
		(provides @1.5-1.15
			(exposed-lower-ident @1.6-1.9
				(text "a1!"))
			(exposed-lower-ident @1.11-1.14
				(text "a2!")))
		(record-field @1.18-1.54 (name "pf")
			(e-string @1.31-1.54
				(e-string-part @1.32-1.53 (raw "../basic-cli/main.roc"))))
		(packages @1.16-1.64
			(record-field @1.18-1.54 (name "pf")
				(e-string @1.31-1.54
					(e-string-part @1.32-1.53 (raw "../basic-cli/main.roc"))))
			(record-field @1.56-1.62 (name "a")
				(e-string @1.59-1.62
					(e-string-part @1.60-1.61 (raw "a"))))))
	(statements))
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
