# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import S exposing[c as
f]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:4-1:5),CloseSquare(1:5-1:6),OpenCurly(1:6-1:7),LowerIdent(1:7-1:8),OpColon(1:8-1:9),KwPlatform(1:9-1:17),StringStart(1:17-1:18),StringPart(1:18-1:18),StringEnd(1:18-1:19),CloseCurly(1:19-1:20),KwImport(1:20-1:26),UpperIdent(1:27-1:28),KwExposing(1:29-1:37),OpenSquare(1:37-1:38),LowerIdent(1:38-1:39),KwAs(1:40-1:42),
LowerIdent(2:1-2:2),CloseSquare(2:2-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(file @1.1-2.3
	(app @1.1-1.20
		(provides @1.4-1.6)
		(record-field @1.7-1.19 (name "f")
			(e-string @1.17-1.19
				(e-string-part @1.18-1.18 (raw ""))))
		(packages @1.6-1.20
			(record-field @1.7-1.19 (name "f")
				(e-string @1.17-1.19
					(e-string-part @1.18-1.18 (raw ""))))))
	(statements
		(s-import @1.20-2.3 (raw "S")
			(exposing
				(exposed-lower-ident @1.38-2.2
					(text "c")
					(as "f"))))))
~~~
# FORMATTED
~~~roc
app [] { f: platform "" }
import S exposing [
	c as f,
]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import @1.20-2.3 (module "S")
		(exposes
			(exposed (name "c") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
