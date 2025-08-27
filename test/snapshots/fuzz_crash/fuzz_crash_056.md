# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform"",r:"",o:""}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:4-1:5),CloseSquare(1:5-1:6),OpenCurly(1:6-1:7),LowerIdent(1:7-1:8),OpColon(1:8-1:9),KwPlatform(1:9-1:17),StringStart(1:17-1:18),StringPart(1:18-1:18),StringEnd(1:18-1:19),Comma(1:19-1:20),LowerIdent(1:20-1:21),OpColon(1:21-1:22),StringStart(1:22-1:23),StringPart(1:23-1:23),StringEnd(1:23-1:24),Comma(1:24-1:25),LowerIdent(1:25-1:26),OpColon(1:26-1:27),StringStart(1:27-1:28),StringPart(1:28-1:28),StringEnd(1:28-1:29),CloseCurly(1:29-1:30),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.30
	(app @1.1-1.30
		(provides @1.4-1.6)
		(record-field @1.7-1.19 (name "f")
			(e-string @1.17-1.19
				(e-string-part @1.18-1.18 (raw ""))))
		(packages @1.6-1.30
			(record-field @1.7-1.19 (name "f")
				(e-string @1.17-1.19
					(e-string-part @1.18-1.18 (raw ""))))
			(record-field @1.20-1.24 (name "r")
				(e-string @1.22-1.24
					(e-string-part @1.23-1.23 (raw ""))))
			(record-field @1.25-1.29 (name "o")
				(e-string @1.27-1.29
					(e-string-part @1.28-1.28 (raw ""))))))
	(statements))
~~~
# FORMATTED
~~~roc
app [] { f: platform "", r: "", o: "" }
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
