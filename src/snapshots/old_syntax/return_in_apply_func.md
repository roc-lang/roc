# META
~~~ini
description=return_in_apply_func
type=expr
~~~
# SOURCE
~~~roc
(
return-3e)g
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - return_in_apply_func.md:2:1:2:8
PARSE ERROR - return_in_apply_func.md:2:10:2:12
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **return-** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**return_in_apply_func.md:2:1:2:8:**
```roc
return-3e)g
```
^^^^^^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**return_in_apply_func.md:2:10:2:12:**
```roc
return-3e)g
```
         ^^


# TOKENS
~~~zig
OpenRound(1:1-1:2),Newline(1:1-1:1),
KwReturn(2:1-2:7),OpBinaryMinus(2:7-2:8),MalformedNumberNoExponentDigits(2:8-2:10),CloseRound(2:10-2:11),LowerIdent(2:11-2:12),EndOfFile(2:12-2:12),
~~~
# PARSE
~~~clojure
(e-malformed @2.10-2.12 (reason "expected_expr_close_round_or_comma"))
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
