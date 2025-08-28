# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0 (
~~~
# EXPECTED
MISSING HEADER - fuzz_hang_001.md:1:1:1:2
PARSE ERROR - fuzz_hang_001.md:1:3:1:4
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

**fuzz_hang_001.md:1:1:1:2:**
```roc
0 (
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_hang_001.md:1:3:1:4:**
```roc
0 (
```
  ^


# TOKENS
~~~zig
Int(1:1-1:2),OpenRound(1:3-1:4),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.4
	(malformed-header @1.1-1.2 (tag "missing_header"))
	(statements
		(s-malformed @1.3-1.4 (tag "statement_unexpected_token"))))
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
