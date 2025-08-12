# META
~~~ini
description=Singleline with comma formatting app
type=file
~~~
# SOURCE
~~~roc
app [a1!, a2!,] { pf: platform "../basic-cli/main.roc", a: "a", }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:9),Comma(1:9-1:10),LowerIdent(1:11-1:14),Comma(1:14-1:15),CloseSquare(1:15-1:16),OpenCurly(1:17-1:18),LowerIdent(1:19-1:21),OpColon(1:21-1:22),KwPlatform(1:23-1:31),StringStart(1:32-1:33),StringPart(1:33-1:54),StringEnd(1:54-1:55),Comma(1:55-1:56),LowerIdent(1:57-1:58),OpColon(1:58-1:59),StringStart(1:60-1:61),StringPart(1:61-1:62),StringEnd(1:62-1:63),Comma(1:63-1:64),CloseCurly(1:65-1:66),EndOfFile(1:66-1:66),
~~~
# PARSE
~~~clojure
(file @1.1-1.66
	(app @1.1-1.66
		(provides @1.5-1.16
			(exposed-lower-ident @1.6-1.9
				(text "a1!"))
			(exposed-lower-ident @1.11-1.14
				(text "a2!")))
		(record-field @1.19-1.55 (name "pf")
			(e-string @1.32-1.55
				(e-string-part @1.33-1.54 (raw "../basic-cli/main.roc"))))
		(packages @1.17-1.66
			(record-field @1.19-1.55 (name "pf")
				(e-string @1.32-1.55
					(e-string-part @1.33-1.54 (raw "../basic-cli/main.roc"))))
			(record-field @1.57-1.63 (name "a")
				(e-string @1.60-1.63
					(e-string-part @1.61-1.62 (raw "a"))))))
	(statements))
~~~
# FORMATTED
~~~roc
app [
	a1!,
	a2!,
] {
	pf: platform "../basic-cli/main.roc",
	a: "a",
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
