# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
*import B as
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_038.md:1:1:1:2
PARSE ERROR - fuzz_crash_038.md:1:2:1:8
MISSING MAIN! FUNCTION - fuzz_crash_038.md:1:1:1:13
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_038.md:1:1:1:2:**
```roc
*import B as
```
^


**PARSE ERROR**
A parsing error occurred: `expected_upper_name_after_import_as`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_038.md:1:2:1:8:**
```roc
*import B as
```
 ^^^^^^


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_crash_038.md:1:1:1:13:**
```roc
*import B as
```
^^^^^^^^^^^^


# TOKENS
~~~zig
OpStar,KwImport,UpperIdent,KwAs,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_upper_name_after_import_as"))))
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
