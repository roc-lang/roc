# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{{0
}}

""
~~~
# EXPECTED
COMPILER DIAGNOSTIC - fuzz_crash_044.md:0:0:0:0
COMPILER DIAGNOSTIC - fuzz_crash_044.md:0:0:0:0
# PROBLEMS
**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'invalid_top_level_statement' is not yet handled in report generation.
**fuzz_crash_044.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'invalid_top_level_statement' is not yet handled in report generation.
**fuzz_crash_044.md:0:0:0:0**

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:4-1:5),CloseSquare(1:5-1:6),OpenCurly(1:6-1:7),LowerIdent(1:7-1:8),OpColon(1:8-1:9),KwPlatform(1:9-1:17),StringStart(1:17-1:18),StringPart(1:18-1:18),StringEnd(1:18-1:19),CloseCurly(1:19-1:20),OpenCurly(1:20-1:21),OpenCurly(1:21-1:22),Int(1:22-1:23),
CloseCurly(2:1-2:2),CloseCurly(2:2-2:3),
StringStart(4:1-4:2),StringPart(4:2-4:2),StringEnd(4:2-4:3),EndOfFile(4:3-4:3),
~~~
# PARSE
~~~clojure
(file @1.1-4.3
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
		(e-block @1.20-2.3
			(statements
				(e-block @1.21-2.2
					(statements
						(e-int @1.22-1.23 (raw "0"))))))
		(e-string @4.1-4.3
			(e-string-part @4.2-4.2 (raw "")))))
~~~
# FORMATTED
~~~roc
app [] { f: platform "" }
{
	{
		0
	}
}

""
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
