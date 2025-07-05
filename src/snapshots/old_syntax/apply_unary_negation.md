# META
~~~ini
description=apply_unary_negation
type=expr
~~~
# SOURCE
~~~roc
-whee  12 foo
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - apply_unary_negation.md:1:1:1:6
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-whee** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**apply_unary_negation.md:1:1:1:6:**
```roc
-whee  12 foo
```
^^^^^


# TOKENS
~~~zig
OpUnaryMinus(1:1-1:2),LowerIdent(1:2-1:6),Int(1:8-1:10),LowerIdent(1:11-1:14),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.6 (reason "expr_unexpected_token"))
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
