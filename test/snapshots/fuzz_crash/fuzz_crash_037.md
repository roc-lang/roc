# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]"\
~~~
# EXPECTED
INVALID ESCAPE SEQUENCE - :0:0:0:0
UNCLOSED STRING - :0:0:0:0
PARSE ERROR - fuzz_crash_037.md:1:9:1:10
PARSE ERROR - fuzz_crash_037.md:1:10:1:11
PARSE ERROR - fuzz_crash_037.md:1:11:1:11
MODULE HEADER DEPRECATED - fuzz_crash_037.md:1:1:1:9
# PROBLEMS
**INVALID ESCAPE SEQUENCE**
This escape sequence is not recognized.

```roc
module[]"\

```


**UNCLOSED STRING**
This string is missing a closing quote.

```roc
module[]"\
```
        ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_037.md:1:9:1:10:**
```roc
module[]"\
```
        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_037.md:1:10:1:11:**
```roc
module[]"\
```
         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_037.md:1:11:1:11:**
```roc
module[]"\
```
          ^


**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**fuzz_crash_037.md:1:1:1:9:**
```roc
module[]"\
```
^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),StringStart(1:9-1:10),MalformedStringPart(1:10-1:11),StringEnd(1:11-1:11),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.11
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(s-malformed @1.9-1.10 (tag "statement_unexpected_token"))
		(s-malformed @1.10-1.11 (tag "statement_unexpected_token"))
		(s-malformed @1.11-1.11 (tag "statement_unexpected_token"))))
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
