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
# PROBLEMS
~~~txt
NIL
~~~
# TOKENS
~~~zig
KwApp(1:1-1:4),Newline(1:1-1:1),
OpenSquare(2:2-2:3),LowerIdent(2:3-2:8),Comma(2:8-2:9),CloseSquare(2:9-2:10),Newline(1:1-1:1),
OpenCurly(3:2-3:3),LowerIdent(3:4-3:6),OpColon(3:6-3:7),KwPlatform(3:8-3:16),StringStart(3:17-3:18),StringPart(3:18-3:29),StringEnd(3:29-3:30),Comma(3:30-3:31),LowerIdent(3:32-3:39),OpColon(3:39-3:40),StringStart(3:41-3:42),StringPart(3:42-3:53),StringEnd(3:53-3:54),Comma(3:54-3:55),CloseCurly(3:56-3:57),EndOfFile(3:57-3:57),
~~~
# PARSE
~~~clojure
(app (1:1-3:57)
	(provides (2:3-2:10) (exposed_item (lower_ident "main!")))
	(record_field (3:4-3:31)
		"pf"
		(string (3:17-3:30) (string_part (3:18-3:29) "../main.roc")))
	(packages (3:2-3:57)
		(record_field (3:4-3:31)
			"pf"
			(string (3:17-3:30) (string_part (3:18-3:29) "../main.roc")))
		(record_field (3:32-3:55)
			"somePkg"
			(string (3:41-3:54) (string_part (3:42-3:53) "../main.roc")))))
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
(can_ir "empty")
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~