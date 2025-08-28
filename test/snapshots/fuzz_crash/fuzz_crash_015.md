# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0o0.0
0_0
0u8.0
0_
~~~
# EXPECTED
LEADING ZERO - :0:0:0:0
MISSING HEADER - fuzz_crash_015.md:1:1:1:4
PARSE ERROR - fuzz_crash_015.md:1:4:1:6
PARSE ERROR - fuzz_crash_015.md:2:1:2:4
PARSE ERROR - fuzz_crash_015.md:3:1:3:4
PARSE ERROR - fuzz_crash_015.md:3:4:3:6
PARSE ERROR - fuzz_crash_015.md:4:1:4:3
# PROBLEMS
**LEADING ZERO**
Numbers cannot have leading zeros.



**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

**fuzz_crash_015.md:1:1:1:4:**
```roc
0o0.0
```
^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_015.md:1:4:1:6:**
```roc
0o0.0
```
   ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_015.md:2:1:2:4:**
```roc
0_0
```
^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_015.md:3:1:3:4:**
```roc
0u8.0
```
^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_015.md:3:4:3:6:**
```roc
0u8.0
```
   ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_015.md:4:1:4:3:**
```roc
0_
```
^^


# TOKENS
~~~zig
Int(1:1-1:4),NoSpaceDotInt(1:4-1:6),
Int(2:1-2:4),
Int(3:1-3:4),NoSpaceDotInt(3:4-3:6),
Int(4:1-4:3),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.3
	(malformed-header @1.1-1.4 (tag "missing_header"))
	(statements
		(s-malformed @1.4-1.6 (tag "statement_unexpected_token"))
		(s-malformed @2.1-2.4 (tag "statement_unexpected_token"))
		(s-malformed @3.1-3.4 (tag "statement_unexpected_token"))
		(s-malformed @3.4-3.6 (tag "statement_unexpected_token"))
		(s-malformed @4.1-4.3 (tag "statement_unexpected_token"))))
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
