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
UNCLOSED STRING - :0:0:0:0
MISSING HEADER - fuzz_crash_003.md:1:1:1:2
PARSE ERROR - fuzz_crash_003.md:1:6:1:6
COMPILER DIAGNOSTIC - fuzz_crash_003.md:0:0:0:0
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
**fuzz_crash_003.md:1:1:1:2:**
```roc
= "te
```
^


**PARSE ERROR**
A parsing error occurred: `string_unclosed`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_003.md:1:6:1:6:**
```roc
= "te
```
     


**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'invalid_top_level_statement' is not yet handled in report generation.
**fuzz_crash_003.md:0:0:0:0**

# TOKENS
~~~zig
OpAssign(1:1-1:2),StringStart(1:3-1:4),StringPart(1:4-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(file @1.1-1.6
	(malformed-header @1.1-1.2 (tag "missing_header"))
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
