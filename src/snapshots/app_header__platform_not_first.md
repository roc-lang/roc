# META
~~~ini
description=App Header - platform not specified first
type=header
~~~
# SOURCE
~~~roc
app
	[main!,]
	{ somePkg: "../main.roc", pf: platform "../main.roc", }
~~~
# PROBLEMS
~~~txt
NIL
~~~
# TOKENS
~~~zig
KwApp(1:1-1:4),Newline(1:1-1:1),
OpenSquare(2:2-2:3),LowerIdent(2:3-2:8),Comma(2:8-2:9),CloseSquare(2:9-2:10),Newline(1:1-1:1),
OpenCurly(3:2-3:3),LowerIdent(3:4-3:11),OpColon(3:11-3:12),StringStart(3:13-3:14),StringPart(3:14-3:25),StringEnd(3:25-3:26),Comma(3:26-3:27),LowerIdent(3:28-3:30),OpColon(3:30-3:31),KwPlatform(3:32-3:40),StringStart(3:41-3:42),StringPart(3:42-3:53),StringEnd(3:53-3:54),Comma(3:54-3:55),CloseCurly(3:56-3:57),EndOfFile(3:57-3:57),
~~~
# PARSE
~~~clojure
(app (1:1-3:57)
	(provides (2:3-2:10) (exposed_item (lower_ident "main!")))
	(record_field (3:28-3:55)
		"pf"
		(string (3:41-3:54) (string_part (3:42-3:53) "../main.roc")))
	(packages (3:2-3:57)
		(record_field (3:4-3:27)
			"somePkg"
			(string (3:13-3:26) (string_part (3:14-3:25) "../main.roc")))
		(record_field (3:28-3:55)
			"pf"
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