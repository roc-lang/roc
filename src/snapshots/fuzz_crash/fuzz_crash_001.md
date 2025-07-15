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
UNEXPECTED TOKEN IN PATTERN - fuzz_crash_001.md:1:4:1:5
PARSE ERROR - fuzz_crash_001.md:1:5:1:5
INVALID STATEMENT - fuzz_crash_001.md:1:3:1:5
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_001.md:1:1:1:3:**
```roc
mo|%
```
^^


**UNEXPECTED TOKEN IN PATTERN**
The token **%** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**fuzz_crash_001.md:1:4:1:5:**
```roc
mo|%
```
   ^


**PARSE ERROR**
A parsing error occurred: `expected_expr_bar`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_001.md:1:5:1:5:**
```roc
mo|%
```
    


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_001.md:1:3:1:5:**
```roc
mo|%
```
  ^^


# TOKENS
~~~zig
LowerIdent(1:1-1:3),OpBar(1:3-1:4),OpPercent(1:4-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(file @1.1-1.5
	(malformed-header @1.1-1.3 (tag "missing_header"))
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
