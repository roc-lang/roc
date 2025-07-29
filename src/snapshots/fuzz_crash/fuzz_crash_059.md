# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import	B	as
G	if 0{}else||0
~~~
# EXPECTED
INVALID STATEMENT - fuzz_crash_059.md:2:3:2:16
# PROBLEMS
**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_059.md:2:3:2:16:**
```roc
G	if 0{}else||0
```
  ^^^^^^^^^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:4-1:5),CloseSquare(1:5-1:6),OpenCurly(1:6-1:7),LowerIdent(1:7-1:8),OpColon(1:8-1:9),KwPlatform(1:9-1:17),StringStart(1:17-1:18),StringPart(1:18-1:18),StringEnd(1:18-1:19),CloseCurly(1:19-1:20),KwImport(1:20-1:26),UpperIdent(1:27-1:28),KwAs(1:29-1:31),
UpperIdent(2:1-2:2),KwIf(2:3-2:5),Int(2:6-2:7),OpenCurly(2:7-2:8),CloseCurly(2:8-2:9),KwElse(2:9-2:13),OpBar(2:13-2:14),OpBar(2:14-2:15),Int(2:15-2:16),EndOfFile(2:16-2:16),
~~~
# PARSE
~~~clojure
(file @1.1-2.16
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
		(s-import @1.20-2.2 (raw "B") (alias "G"))
		(e-if-then-else @2.3-2.16
			(e-int @2.6-2.7 (raw "0"))
			(e-record @2.7-2.9)
			(e-lambda @2.13-2.16
				(args)
				(e-int @2.15-2.16 (raw "0"))))))
~~~
# FORMATTED
~~~roc
app [] { f: platform "" }
import B as
G
if 0 {} else || 0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import @1.20-2.2 (module "B") (alias "G")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
