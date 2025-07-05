# META
~~~ini
description=pnc_apply_neg_pattern
type=expr
~~~
# SOURCE
~~~roc
(-8)():C
8
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - pnc_apply_neg_pattern.md:1:2:1:4
expected_expr_close_round_or_comma - pnc_apply_neg_pattern.md:1:4:1:6
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-8** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**pnc_apply_neg_pattern.md:1:2:1:4:**
```roc
(-8)():C
```
 ^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**pnc_apply_neg_pattern.md:1:4:1:6:**
```roc
(-8)():C
```
   ^^


# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBinaryMinus(1:2-1:3),Int(1:3-1:4),CloseRound(1:4-1:5),NoSpaceOpenRound(1:5-1:6),CloseRound(1:6-1:7),OpColon(1:7-1:8),UpperIdent(1:8-1:9),Newline(1:1-1:1),
Int(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-malformed @1.4-1.6 (reason "expected_expr_close_round_or_comma"))
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
