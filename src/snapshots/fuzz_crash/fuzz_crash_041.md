# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}|(0,)|||0
~~~
# EXPECTED
COMPILER DIAGNOSTIC - fuzz_crash_041.md:0:0:0:0
# PROBLEMS
**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'invalid_top_level_statement' is not yet handled in report generation.
**fuzz_crash_041.md:0:0:0:0**

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:4-1:5),CloseSquare(1:5-1:6),OpenCurly(1:6-1:7),LowerIdent(1:7-1:8),OpColon(1:8-1:9),KwPlatform(1:9-1:17),StringStart(1:17-1:18),StringPart(1:18-1:18),StringEnd(1:18-1:19),CloseCurly(1:19-1:20),OpBar(1:20-1:21),NoSpaceOpenRound(1:21-1:22),Int(1:22-1:23),Comma(1:23-1:24),CloseRound(1:24-1:25),OpBar(1:25-1:26),OpBar(1:26-1:27),OpBar(1:27-1:28),Int(1:28-1:29),EndOfFile(1:29-1:29),
~~~
# PARSE
~~~clojure
(file @1.1-1.29
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
		(e-lambda @1.20-1.29
			(args
				(p-tuple @1.21-1.25
					(p-int @1.22-1.23 (raw "0"))))
			(e-lambda @1.26-1.29
				(args)
				(e-int @1.28-1.29 (raw "0"))))))
~~~
# FORMATTED
~~~roc
app [] { f: platform "" }
|
	(
		0,
	),
| || 0
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
