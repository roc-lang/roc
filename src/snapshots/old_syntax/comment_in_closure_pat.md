# META
~~~ini
description=comment_in_closure_pat
type=expr
~~~
# SOURCE
~~~roc
\L#
 i->-e
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - comment_in_closure_pat.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\L** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**comment_in_closure_pat.md:1:1:1:3:**
```roc
\L#
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),UpperIdent(1:2-1:3),Newline(1:4-1:4),
LowerIdent(2:2-2:3),OpArrow(2:3-2:5),OpBinaryMinus(2:5-2:6),LowerIdent(2:6-2:7),EndOfFile(2:7-2:7),
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
