# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
||1
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
MISSING HEADER - fuzz_crash_008.md:1:1:1:2
PARSE ERROR - fuzz_crash_008.md:1:5:1:5
COMPILER DIAGNOSTIC - fuzz_crash_008.md:0:0:0:0
# PROBLEMS
**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.

**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_008.md:1:1:1:2:**
```roc
||1
```
^


**PARSE ERROR**
A parsing error occurred: `expected_expr_bar`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_008.md:1:5:1:5:**
```roc
||1
```
    


**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'invalid_top_level_statement' is not yet handled in report generation.
**fuzz_crash_008.md:0:0:0:0**

# TOKENS
~~~zig
OpBar(1:1-1:2),OpBar(1:3-1:4),Int(1:4-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(file @1.1-1.5
	(malformed-header @1.1-1.2 (tag "missing_header"))
	(statements
		(e-malformed @1.5-1.5 (reason "expected_expr_bar"))))
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
