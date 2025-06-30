# META
~~~ini
description=capture_body_parens_comment
type=expr
~~~
# SOURCE
~~~roc
\L->(E
#
)
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\L** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**capture_body_parens_comment.md:1:1:1:3:**
```roc
\L->(E
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),UpperIdent(1:2-1:3),OpArrow(1:3-1:5),NoSpaceOpenRound(1:5-1:6),UpperIdent(1:6-1:7),Newline(1:1-1:1),
Newline(2:2-2:2),
CloseRound(3:1-3:2),EndOfFile(3:2-3:2),
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
