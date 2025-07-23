# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[){..0,)
~~~
# EXPECTED
MISMATCHED BRACE - :0:0:0:0
MISMATCHED BRACE - :0:0:0:0
COMPILER DIAGNOSTIC - fuzz_crash_053.md:0:0:0:0
# PROBLEMS
**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'invalid_top_level_statement' is not yet handled in report generation.
**fuzz_crash_053.md:0:0:0:0**

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),OpenCurly(1:9-1:10),DoubleDot(1:10-1:12),Int(1:12-1:13),Comma(1:13-1:14),CloseCurly(1:14-1:15),EndOfFile(1:15-1:15),
~~~
# PARSE
~~~clojure
(file @1.1-1.15
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(e-record @1.9-1.15
			(ext
				(e-int @1.12-1.13 (raw "0"))))))
~~~
# FORMATTED
~~~roc
module []
{
	..0,
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
