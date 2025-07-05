# META
~~~ini
description=negate_apply_parens_comment
type=expr
~~~
# SOURCE
~~~roc
-((4
4)4)
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - negate_apply_parens_comment.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**negate_apply_parens_comment.md:1:1:1:3:**
```roc
-((4
```
^^


# TOKENS
~~~zig
OpUnaryMinus(1:1-1:2),NoSpaceOpenRound(1:2-1:3),NoSpaceOpenRound(1:3-1:4),Int(1:4-1:5),Newline(1:1-1:1),
Int(2:1-2:2),CloseRound(2:2-2:3),Int(2:3-2:4),CloseRound(2:4-2:5),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.3 (reason "expr_unexpected_token"))
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
