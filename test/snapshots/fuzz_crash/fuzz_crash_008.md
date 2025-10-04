# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
||1
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
PARSE ERROR - fuzz_crash_008.md:1:1:1:2
PARSE ERROR - fuzz_crash_008.md:1:3:1:4
PARSE ERROR - fuzz_crash_008.md:1:4:1:5
TYPE MODULE MISSING MATCHING TYPE - fuzz_crash_008.md:1:1:1:5
# PROBLEMS
**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.



**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_008.md:1:1:1:2:**
```roc
||1
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_008.md:1:3:1:4:**
```roc
||1
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_008.md:1:4:1:5:**
```roc
||1
```
   ^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `fuzz_crash_008`.roc, but no top-level type declaration named `fuzz_crash_008` was found.

Add either:
`fuzz_crash_008 := ...` (nominal type)
or:
`fuzz_crash_008 : ...` (type alias)
**fuzz_crash_008.md:1:1:1:5:**
```roc
||1
```
^^^^


# TOKENS
~~~zig
OpBar(1:1-1:2),OpBar(1:3-1:4),Int(1:4-1:5),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.5
	(type-module @1.1-1.2)
	(statements
		(s-malformed @1.1-1.2 (tag "statement_unexpected_token"))
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
