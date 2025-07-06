# META
~~~ini
description=unary_not
type=expr
~~~
# SOURCE
~~~roc
!blah
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - unary_not.md:1:1:1:6
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **!blah** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unary_not.md:1:1:1:6:**
```roc
!blah
```
^^^^^


# TOKENS
~~~zig
OpBang(1:1-1:2),LowerIdent(1:2-1:6),EndOfFile(1:6-1:6),
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
