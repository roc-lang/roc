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
PARSE ERROR - fuzz_crash_014.md:1:1:1:3
PARSE ERROR - fuzz_crash_014.md:1:3:1:5
PARSE ERROR - fuzz_crash_014.md:2:1:2:6
PARSE ERROR - fuzz_crash_014.md:3:1:3:5
MISSING MAIN! FUNCTION - fuzz_crash_014.md:1:1:3:5
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

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


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_crash_014.md:1:1:3:5:**
```roc
0b.0
0bu22
0u22
```


# TOKENS
~~~zig
MalformedNumberNoDigits,NoSpaceDotInt,
MalformedNumberNoDigits,
MalformedNumberBadSuffix,
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
