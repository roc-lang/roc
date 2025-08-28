# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
mo|%
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_001.md:1:1:1:3
PARSE ERROR - fuzz_crash_001.md:1:3:1:4
PARSE ERROR - fuzz_crash_001.md:1:4:1:5
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

**fuzz_crash_001.md:1:1:1:3:**
```roc
mo|%
```
^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_001.md:1:3:1:4:**
```roc
mo|%
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_001.md:1:4:1:5:**
```roc
mo|%
```
   ^


# TOKENS
~~~zig
LowerIdent(1:1-1:3),OpBar(1:3-1:4),OpPercent(1:4-1:5),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.5
	(malformed-header @1.1-1.3 (tag "missing_header"))
	(statements
		(s-malformed @1.3-1.4 (tag "statement_unexpected_token"))
		(s-malformed @1.4-1.5 (tag "statement_unexpected_token"))))
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
