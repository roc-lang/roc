# META
~~~ini
description=Higher-order function type annotation without outer parentheses
type=file
~~~
# SOURCE
~~~roc
module[}|0
as s|||0
~~~
# EXPECTED
MISMATCHED BRACE - :0:0:0:0
INVALID STATEMENT - fuzz_crash_062.md:1:9:2:9
# PROBLEMS
**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_062.md:1:9:2:9:**
```roc
module[}|0
as s|||0
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),OpBar(1:9-1:10),Int(1:10-1:11),
KwAs(2:1-2:3),LowerIdent(2:4-2:5),OpBar(2:5-2:6),OpBar(2:6-2:7),OpBar(2:7-2:8),Int(2:8-2:9),EndOfFile(2:9-2:9),
~~~
# PARSE
~~~clojure
(file @1.1-2.9
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(e-lambda @1.9-2.9
			(args
				(p-as @1.10-2.3 (name "s")
					(p-int @1.10-1.11 (raw "0"))))
			(e-lambda @2.6-2.9
				(args)
				(e-int @2.8-2.9 (raw "0"))))))
~~~
# FORMATTED
~~~roc
module []
|
	0 as s,
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
