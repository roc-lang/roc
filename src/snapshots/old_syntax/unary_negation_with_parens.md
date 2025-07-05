# META
~~~ini
description=unary_negation_with_parens
type=expr
~~~
# SOURCE
~~~roc
-(whee  12 foo)
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - unary_negation_with_parens.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unary_negation_with_parens.md:1:1:1:3:**
```roc
-(whee  12 foo)
```
^^


# TOKENS
~~~zig
OpUnaryMinus(1:1-1:2),NoSpaceOpenRound(1:2-1:3),LowerIdent(1:3-1:7),Int(1:9-1:11),LowerIdent(1:12-1:15),CloseRound(1:15-1:16),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
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
