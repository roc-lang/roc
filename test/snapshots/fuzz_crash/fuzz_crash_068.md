# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]({0})
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_068.md:1:9:1:10
PARSE ERROR - fuzz_crash_068.md:1:10:1:11
PARSE ERROR - fuzz_crash_068.md:1:11:1:12
PARSE ERROR - fuzz_crash_068.md:1:12:1:13
PARSE ERROR - fuzz_crash_068.md:1:13:1:14
MODULE HEADER DEPRECATED - fuzz_crash_068.md:1:1:1:9
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_068.md:1:9:1:10:**
```roc
module[]({0})
```
        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_068.md:1:10:1:11:**
```roc
module[]({0})
```
         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_068.md:1:11:1:12:**
```roc
module[]({0})
```
          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_068.md:1:12:1:13:**
```roc
module[]({0})
```
           ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_068.md:1:13:1:14:**
```roc
module[]({0})
```
            ^


**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**fuzz_crash_068.md:1:1:1:9:**
```roc
module[]({0})
```
^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),NoSpaceOpenRound(1:9-1:10),OpenCurly(1:10-1:11),Int(1:11-1:12),CloseCurly(1:12-1:13),CloseRound(1:13-1:14),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.14
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(s-malformed @1.9-1.10 (tag "statement_unexpected_token"))
		(s-malformed @1.10-1.11 (tag "statement_unexpected_token"))
		(s-malformed @1.11-1.12 (tag "statement_unexpected_token"))
		(s-malformed @1.12-1.13 (tag "statement_unexpected_token"))
		(s-malformed @1.13-1.14 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module []
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
