# META
~~~ini
description=return_with_after
type=expr
~~~
# SOURCE
~~~roc
return-1#
X
s
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **return-** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**return_with_after.md:1:1:1:8:**
```roc
return-1#
```
^^^^^^^


# TOKENS
~~~zig
KwReturn(1:1-1:7),OpBinaryMinus(1:7-1:8),Int(1:8-1:9),Newline(1:10-1:10),
UpperIdent(2:1-2:2),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.8 (reason "expr_unexpected_token"))
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
