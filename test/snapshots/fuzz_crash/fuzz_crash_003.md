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
TYPE MODULE MISSING MATCHING TYPE - fuzz_crash_003.md:1:1:1:6
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


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `fuzz_crash_003`.roc, but no top-level type declaration named `fuzz_crash_003` was found.

Add either:
`fuzz_crash_003 := ...` (nominal type)
or:
`fuzz_crash_003 : ...` (type alias)
**fuzz_crash_003.md:1:1:1:6:**
```roc
= "te
```
^^^^^


# TOKENS
~~~zig
OpAssign(1:1-1:2),StringStart(1:3-1:4),StringPart(1:4-1:6),StringEnd(1:6-1:6),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.6
	(type-module @1.1-1.2)
	(statements
		(s-malformed @1.1-1.2 (tag "statement_unexpected_token"))
		(s-malformed @1.3-1.4 (tag "statement_unexpected_token"))
		(s-malformed @1.4-1.6 (tag "statement_unexpected_token"))
		(s-malformed @1.6-1.6 (tag "statement_unexpected_token"))))
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
