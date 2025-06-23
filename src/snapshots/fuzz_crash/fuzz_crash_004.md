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
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_004.md:1:1:1:2:**
```roc
F
```


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