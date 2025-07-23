# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}('
)
~~~
# EXPECTED
MISMATCHED BRACE - :0:0:0:0
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_039.md:1:10:1:11
COMPILER DIAGNOSTIC - fuzz_crash_039.md:0:0:0:0
# PROBLEMS
**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**UNEXPECTED TOKEN IN EXPRESSION**
The token **'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_039.md:1:10:1:11:**
```roc
module[}('
```
         ^


**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'invalid_top_level_statement' is not yet handled in report generation.
**fuzz_crash_039.md:0:0:0:0**

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),NoSpaceOpenRound(1:9-1:10),MalformedSingleQuoteUnclosed(1:10-1:11),
CloseRound(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(file @1.1-2.2
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(e-tuple @1.9-2.2
			(e-malformed @1.10-1.11 (reason "expr_unexpected_token")))))
~~~
# FORMATTED
~~~roc
module []
(
	,
)
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
