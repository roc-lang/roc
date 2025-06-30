# META
~~~ini
description=comment_in_closure_pat_apply
type=expr
~~~
# SOURCE
~~~roc
\L,M#
 Q->f8
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\L** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**comment_in_closure_pat_apply.md:1:1:1:3:**
```roc
\L,M#
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),UpperIdent(1:2-1:3),Comma(1:3-1:4),UpperIdent(1:4-1:5),Newline(1:6-1:6),
UpperIdent(2:2-2:3),OpArrow(2:3-2:5),LowerIdent(2:5-2:7),EndOfFile(2:7-2:7),
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
