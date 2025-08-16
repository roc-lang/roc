# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
ff8.8.d
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_007.md:1:1:1:4
PARSE ERROR - fuzz_crash_007.md:1:4:1:6
PARSE ERROR - fuzz_crash_007.md:1:6:1:8
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

**fuzz_crash_007.md:1:1:1:4:**
```roc
ff8.8.d
```
^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_007.md:1:4:1:6:**
```roc
ff8.8.d
```
   ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_007.md:1:6:1:8:**
```roc
ff8.8.d
```
     ^^


# TOKENS
~~~zig
LowerIdent(1:1-1:4),NoSpaceDotInt(1:4-1:6),NoSpaceDotLowerIdent(1:6-1:8),EndOfFile(1:8-1:8),
~~~
# PARSE
~~~clojure
(file @1.1-1.8
	(malformed-header @1.1-1.4 (tag "missing_header"))
	(statements
		(s-malformed @1.4-1.6 (tag "statement_unexpected_token"))
		(s-malformed @1.6-1.8 (tag "statement_unexpected_token"))))
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
