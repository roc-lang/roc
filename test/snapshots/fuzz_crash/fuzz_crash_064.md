# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc

~~~
# EXPECTED
MISSING HEADER - fuzz_crash_064.md:2:1:2:1
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

**fuzz_crash_064.md:2:1:2:1:**
```roc

```
^


# TOKENS
~~~zig
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @2.1-2.1
	(malformed-header @2.1-2.1 (tag "missing_header"))
	(statements))
~~~
# FORMATTED
~~~roc
MALFORMED INPUT
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
