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
PARSE ERROR - fuzz_crash_003.md:1:1:1:2
PARSE ERROR - fuzz_crash_003.md:1:3:1:4
PARSE ERROR - fuzz_crash_003.md:1:4:1:6
PARSE ERROR - fuzz_crash_003.md:1:6:1:6
MISSING MAIN! FUNCTION - fuzz_crash_003.md:1:1:1:6
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

```roc
= "te
```
  ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_003.md:1:1:1:2:**
```roc
= "te
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_003.md:1:3:1:4:**
```roc
= "te
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_003.md:1:4:1:6:**
```roc
= "te
```
   ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_003.md:1:6:1:6:**
```roc
= "te
```
     ^


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_crash_003.md:1:1:1:6:**
```roc
= "te
```
^^^^^


# TOKENS
~~~zig
OpAssign,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
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
