# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module P]F
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_011.md:1:8:1:9
PARSE ERROR - fuzz_crash_011.md:1:9:1:10
PARSE ERROR - fuzz_crash_011.md:2:1:2:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `header_expected_open_square`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_011.md:1:8:1:9:**
```roc
module P]F
```
       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_011.md:1:9:1:10:**
```roc
module P]F
```
        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_011.md:1:10:1:11:**
```roc
module P]F
```
         ^


# TOKENS
~~~zig
KwModule,UpperIdent,CloseSquare,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "header_expected_open_square"))
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
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
