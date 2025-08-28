# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0b.0
0bu22
0u22
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_014.md:1:1:1:3
PARSE ERROR - fuzz_crash_014.md:1:3:1:5
PARSE ERROR - fuzz_crash_014.md:2:1:2:6
PARSE ERROR - fuzz_crash_014.md:3:1:3:5
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

**fuzz_crash_014.md:1:1:1:3:**
```roc
0b.0
```
^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_014.md:1:3:1:5:**
```roc
0b.0
```
  ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_014.md:2:1:2:6:**
```roc
0bu22
```
^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_014.md:3:1:3:5:**
```roc
0u22
```
^^^^


# TOKENS
~~~zig
MalformedNumberNoDigits(1:1-1:3),NoSpaceDotInt(1:3-1:5),
MalformedNumberNoDigits(2:1-2:6),
MalformedNumberBadSuffix(3:1-3:5),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.5
	(malformed-header @1.1-1.3 (tag "missing_header"))
	(statements
		(s-malformed @1.3-1.5 (tag "statement_unexpected_token"))
		(s-malformed @2.1-2.6 (tag "statement_unexpected_token"))
		(s-malformed @3.1-3.5 (tag "statement_unexpected_token"))))
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
