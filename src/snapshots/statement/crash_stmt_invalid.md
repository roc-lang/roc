# META
~~~ini
description=Crash statement with invalid non-string argument
type=statement
~~~
# SOURCE
~~~roc
crash 42
~~~
# EXPECTED
COMPILER DIAGNOSTIC - crash_stmt_invalid.md:0:0:0:0
# PROBLEMS
**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'crash_expects_string' is not yet handled in report generation.
**crash_stmt_invalid.md:0:0:0:0**

# TOKENS
~~~zig
KwCrash(1:1-1:6),Int(1:7-1:9),EndOfFile(1:9-1:9),
~~~
# PARSE
~~~clojure
(s-crash @1.1-1.9
	(e-int @1.7-1.9 (raw "42")))
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
