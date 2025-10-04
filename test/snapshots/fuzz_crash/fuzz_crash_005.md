# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
modu
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_005.md:1:1:1:5
TYPE MODULE MISSING MATCHING TYPE - fuzz_crash_005.md:1:1:1:5
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_005.md:1:1:1:5:**
```roc
modu
```
^^^^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `fuzz_crash_005`.roc, but no top-level type declaration named `fuzz_crash_005` was found.

Add either:
`fuzz_crash_005 := ...` (nominal type)
or:
`fuzz_crash_005 : ...` (type alias)
**fuzz_crash_005.md:1:1:1:5:**
```roc
modu
```
^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.5
	(type-module @1.1-1.5)
	(statements
		(s-malformed @1.1-1.5 (tag "statement_unexpected_token"))))
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
