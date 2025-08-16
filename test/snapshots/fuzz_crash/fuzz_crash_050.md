# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]_0={
)
 
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_050.md:2:1:2:2
PARSE ERROR - fuzz_crash_050.md:3:2:3:2
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_050.md:2:1:2:2:**
```roc
)
```
^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_050.md:3:2:3:2:**
```roc
 
```
 ^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),NamedUnderscore(1:9-1:11),OpAssign(1:11-1:12),OpenCurly(1:12-1:13),
CloseRound(2:1-2:2),
EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(file @1.1-2.2
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(s-decl @1.9-2.2
			(p-ident @1.9-1.11 (raw "_0"))
			(e-block @1.12-2.2
				(statements
					(e-malformed @2.1-2.2 (reason "expr_unexpected_token")))))))
~~~
# FORMATTED
~~~roc
module []
_0 = {
	
}

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.9-1.11 (ident "_0"))
		(e-block @1.12-2.2
			(e-empty_record @1.12-2.2))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.9-1.11 (type "{}")))
	(expressions
		(expr @1.12-2.2 (type "{}"))))
~~~
