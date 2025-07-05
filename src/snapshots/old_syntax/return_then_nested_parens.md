# META
~~~ini
description=return_then_nested_parens
type=expr
~~~
# SOURCE
~~~roc
return n
((0
)#
)
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - return_then_nested_parens.md:1:1:1:9
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **return n** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**return_then_nested_parens.md:1:1:1:9:**
```roc
return n
```
^^^^^^^^


# TOKENS
~~~zig
KwReturn(1:1-1:7),LowerIdent(1:8-1:9),Newline(1:1-1:1),
OpenRound(2:1-2:2),NoSpaceOpenRound(2:2-2:3),Int(2:3-2:4),Newline(1:1-1:1),
CloseRound(3:1-3:2),Newline(3:3-3:3),
CloseRound(4:1-4:2),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.9 (reason "expr_unexpected_token"))
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
