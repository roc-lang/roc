# META
~~~ini
description=neg_float_literal_pnc_apply_pat
type=expr
~~~
# SOURCE
~~~roc
(-8.)():C
p
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - neg_float_literal_pnc_apply_pat.md:1:2:1:4
PARSE ERROR - neg_float_literal_pnc_apply_pat.md:1:5:1:7
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-8** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**neg_float_literal_pnc_apply_pat.md:1:2:1:4:**
```roc
(-8.)():C
```
 ^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**neg_float_literal_pnc_apply_pat.md:1:5:1:7:**
```roc
(-8.)():C
```
    ^^


# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBinaryMinus(1:2-1:3),Int(1:3-1:4),Dot(1:4-1:5),CloseRound(1:5-1:6),NoSpaceOpenRound(1:6-1:7),CloseRound(1:7-1:8),OpColon(1:8-1:9),UpperIdent(1:9-1:10),Newline(1:1-1:1),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-malformed @1.5-1.7 (reason "expected_expr_close_round_or_comma"))
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
