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
KwModule,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceDotLowerIdent,OpColon,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "header_expected_open_square"))
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
