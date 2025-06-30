# META
~~~ini
description=neg_nested_parens
type=expr
~~~
# SOURCE
~~~roc
-(0(1
d))
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**neg_nested_parens.md:1:1:1:3:**
```roc
-(0(1
```
^^


# TOKENS
~~~zig
OpUnaryMinus(1:1-1:2),NoSpaceOpenRound(1:2-1:3),Int(1:3-1:4),NoSpaceOpenRound(1:4-1:5),Int(1:5-1:6),Newline(1:1-1:1),
LowerIdent(2:1-2:2),CloseRound(2:2-2:3),CloseRound(2:3-2:4),EndOfFile(2:4-2:4),
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
