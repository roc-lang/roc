# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
module(a).h:s
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_055.md:1:7:1:8
PARSE ERROR - fuzz_crash_055.md:1:8:1:9
PARSE ERROR - fuzz_crash_055.md:1:9:1:10
PARSE ERROR - fuzz_crash_055.md:1:10:1:12
PARSE ERROR - fuzz_crash_055.md:1:12:1:13
PARSE ERROR - fuzz_crash_055.md:1:13:1:14
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `header_expected_open_square`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_055.md:1:7:1:8:**
```roc
module(a).h:s
```
      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_055.md:1:8:1:9:**
```roc
module(a).h:s
```
       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_055.md:1:9:1:10:**
```roc
module(a).h:s
```
        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_055.md:1:10:1:12:**
```roc
module(a).h:s
```
         ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_055.md:1:12:1:13:**
```roc
module(a).h:s
```
           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_055.md:1:13:1:14:**
```roc
module(a).h:s
```
            ^


# TOKENS
~~~zig
KwModule(1:1-1:7),NoSpaceOpenRound(1:7-1:8),LowerIdent(1:8-1:9),CloseRound(1:9-1:10),NoSpaceDotLowerIdent(1:10-1:12),OpColon(1:12-1:13),LowerIdent(1:13-1:14),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.14
	(malformed-header @1.7-1.8 (tag "header_expected_open_square"))
	(statements
		(s-malformed @1.8-1.9 (tag "statement_unexpected_token"))
		(s-malformed @1.9-1.10 (tag "statement_unexpected_token"))
		(s-malformed @1.10-1.12 (tag "statement_unexpected_token"))
		(s-malformed @1.12-1.13 (tag "statement_unexpected_token"))
		(s-malformed @1.13-1.14 (tag "statement_unexpected_token"))))
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
