# META
~~~ini
description=parens_apply_not_parens
type=expr
~~~
# SOURCE
~~~roc
(!(4
)4)
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **!(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**parens_apply_not_parens.md:1:2:1:4:**
```roc
(!(4
```
 ^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**parens_apply_not_parens.md:2:1:2:3:**
```roc
)4)
```
^^


# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBang(1:2-1:3),NoSpaceOpenRound(1:3-1:4),Int(1:4-1:5),Newline(1:1-1:1),
CloseRound(2:1-2:2),Int(2:2-2:3),CloseRound(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-malformed @2.1-2.3 (reason "expected_expr_close_round_or_comma"))
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
