# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]()0     .t
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_070.md:1:17:1:19
INVALID STATEMENT - fuzz_crash_070.md:1:9:1:11
INVALID STATEMENT - fuzz_crash_070.md:1:11:1:19
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expr_dot_suffix_not_allowed`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_070.md:1:17:1:19:**
```roc
module[]()0     .t
```
                ^^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_070.md:1:9:1:11:**
```roc
module[]()0     .t
```
        ^^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_070.md:1:11:1:19:**
```roc
module[]()0     .t
```
          ^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),NoSpaceOpenRound(1:9-1:10),CloseRound(1:10-1:11),Int(1:11-1:12),DotLowerIdent(1:17-1:19),EndOfFile(1:19-1:19),
~~~
# PARSE
~~~clojure
(file @1.1-1.19
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(e-tuple @1.9-1.11)
		(e-malformed @1.17-1.19 (reason "expr_dot_suffix_not_allowed"))))
~~~
# FORMATTED
~~~roc
module []
()

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
