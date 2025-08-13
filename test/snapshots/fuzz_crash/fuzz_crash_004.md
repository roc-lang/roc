# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
F
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_004.md:1:1:1:2
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
^


# TOKENS
~~~zig
UpperIdent(1:1-1:2),EndOfFile(1:2-1:2),
~~~
# PARSE
~~~clojure
(file @1.1-1.2
	(malformed-header @1.1-1.2 (tag "missing_header"))
	(statements))
~~~
# FORMATTED
~~~roc

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
