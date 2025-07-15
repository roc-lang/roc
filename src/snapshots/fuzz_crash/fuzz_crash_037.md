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
PARSE ERROR - fuzz_crash_037.md:1:11:1:11
INVALID STATEMENT - fuzz_crash_037.md:1:9:1:11
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

**PARSE ERROR**
A parsing error occurred: `string_unclosed`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_037.md:1:11:1:11:**
```roc
module[]"\
```
          


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_037.md:1:9:1:11:**
```roc
module[]"\
```
        ^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),StringStart(1:9-1:10),StringPart(1:10-1:11),EndOfFile(1:11-1:11),
~~~
# PARSE
~~~clojure
(file @1.1-1.11
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(e-string @1.9-1.11
			(e-string-part @1.10-1.11 (raw "\")))))
~~~
# FORMATTED
~~~roc
module []
"\"
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
