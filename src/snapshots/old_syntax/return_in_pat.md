# META
~~~ini
description=return_in_pat fail
type=expr
~~~
# SOURCE
~~~roc
(
return e
t)=t
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - return_in_pat.md:2:1:2:9
expected_expr_close_round_or_comma - return_in_pat.md:3:2:3:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **return e** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**return_in_pat.md:2:1:2:9:**
```roc
return e
```
^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**return_in_pat.md:3:2:3:4:**
```roc
t)=t
```
 ^^


# TOKENS
~~~zig
OpenRound(1:1-1:2),Newline(1:1-1:1),
KwReturn(2:1-2:7),LowerIdent(2:8-2:9),Newline(1:1-1:1),
LowerIdent(3:1-3:2),CloseRound(3:2-3:3),OpAssign(3:3-3:4),LowerIdent(3:4-3:5),EndOfFile(3:5-3:5),
~~~
# PARSE
~~~clojure
(e-malformed @3.2-3.4 (reason "expected_expr_close_round_or_comma"))
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
