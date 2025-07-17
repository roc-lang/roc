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
UNCLOSED SINGLE QUOTE - :0:0:0:0
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_039.md:1:10:1:11
INVALID STATEMENT - fuzz_crash_039.md:1:9:2:2
# PROBLEMS
**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**UNCLOSED SINGLE QUOTE**
This character literal is missing a closing single quote.

**UNEXPECTED TOKEN IN EXPRESSION**
The token **'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_039.md:1:10:1:11:**
```roc
module[}('
```
         ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_039.md:1:9:2:2:**
```roc
module[}('
)
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),NoSpaceOpenRound(1:9-1:10),MalformedSingleQuote(1:10-1:11),
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
