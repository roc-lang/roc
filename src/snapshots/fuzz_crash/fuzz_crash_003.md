# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
= "te
~~~
# EXPECTED
UnclosedString - fuzz_crash_003.md:1:4:1:6
missing_header - fuzz_crash_003.md:1:1:1:4
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_003.md:1:1:1:4:**
```roc
= "te
```
^^^


# TOKENS
~~~zig
OpAssign(1:1-1:2),StringStart(1:3-1:4),StringPart(1:4-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(file @1.1-1.6
	(malformed-header @1.1-1.4 (tag "missing_header"))
	(statements
		(e-string @1.3-1.6
			(e-string-part @1.4-1.6 (raw "te")))))
~~~
# FORMATTED
~~~roc
"te"
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
