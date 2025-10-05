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
PARSE ERROR - fuzz_crash_001.md:1:1:1:3
PARSE ERROR - fuzz_crash_001.md:1:3:1:4
PARSE ERROR - fuzz_crash_001.md:1:4:1:5
TYPE MODULE MISSING MATCHING TYPE - fuzz_crash_001.md:1:1:1:5
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_001.md:1:1:1:3:**
```roc
mo|%
```
^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_001.md:1:3:1:4:**
```roc
mo|%
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_001.md:1:4:1:5:**
```roc
mo|%
```
   ^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `fuzz_crash_001`.roc, but no top-level type declaration named `fuzz_crash_001` was found.

Add either:
`fuzz_crash_001 := ...` (nominal type)
or:
`fuzz_crash_001 : ...` (type alias)
**fuzz_crash_001.md:1:1:1:5:**
```roc
mo|%
```
^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:3),OpBar(1:3-1:4),OpPercent(1:4-1:5),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.5
	(type-module @1.1-1.3)
	(statements
		(s-malformed @1.1-1.3 (tag "statement_unexpected_token"))
		(s-malformed @1.3-1.4 (tag "statement_unexpected_token"))
		(s-malformed @1.4-1.5 (tag "statement_unexpected_token"))))
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
