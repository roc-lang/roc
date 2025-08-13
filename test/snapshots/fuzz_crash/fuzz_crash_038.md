# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
*import B as
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_038.md:1:1:1:2
PARSE ERROR - fuzz_crash_038.md:1:2:1:8
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_038.md:1:1:1:2:**
```roc
*import B as
```
^


**PARSE ERROR**
A parsing error occurred: `expected_upper_name_after_import_as`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_038.md:1:2:1:8:**
```roc
*import B as
```
 ^^^^^^


# TOKENS
~~~zig
OpStar(1:1-1:2),KwImport(1:2-1:8),UpperIdent(1:9-1:10),KwAs(1:11-1:13),EndOfFile(1:13-1:13),
~~~
# PARSE
~~~clojure
(file @1.1-1.13
	(malformed-header @1.1-1.2 (tag "missing_header"))
	(statements
		(s-malformed @1.2-1.13 (tag "expected_upper_name_after_import_as"))))
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
