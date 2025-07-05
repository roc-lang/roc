# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
modu
~~~
# EXPECTED
missing_header - fuzz_crash_005.md:1:1:1:5
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_005.md:1:1:1:5:**
```roc
modu
```
^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(file @1.1-1.5
	(malformed-header @1.1-1.5 (tag "missing_header"))
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
