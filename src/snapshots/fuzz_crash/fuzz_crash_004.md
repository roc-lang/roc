# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
F
~~~
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header like 'module [main]' or 'app [main] { pf: platform "..." }'.

# TOKENS
~~~zig
UpperIdent(1:1-1:2),EndOfFile(1:2-1:2),
~~~
# PARSE
~~~clojure
(file (1:1-1:2)
	(malformed_header (1:1-1:2) "missing_header")
	(statements))
~~~
# FORMATTED
~~~roc

~~~
# CANONICALIZE
~~~clojure
(can_ir "empty")
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~