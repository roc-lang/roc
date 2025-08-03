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
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_070.md:1:17:1:19
INVALID STATEMENT - fuzz_crash_070.md:1:9:1:11
INVALID STATEMENT - fuzz_crash_070.md:1:11:1:12
INVALID STATEMENT - fuzz_crash_070.md:1:17:1:19
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **.t** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

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

**fuzz_crash_070.md:1:11:1:12:**
```roc
module[]()0     .t
```
          ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_070.md:1:17:1:19:**
```roc
module[]()0     .t
```
                ^^


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
		(e-int @1.11-1.12 (raw "0"))
		(e-malformed @1.17-1.19 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module []
()
0

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
