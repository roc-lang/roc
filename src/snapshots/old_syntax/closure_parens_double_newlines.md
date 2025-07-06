# META
~~~ini
description=closure_parens_double_newlines
type=expr
~~~
# SOURCE
~~~roc
\L((z
)
)->42
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - closure_parens_double_newlines.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\L** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**closure_parens_double_newlines.md:1:1:1:3:**
```roc
\L((z
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),UpperIdent(1:2-1:3),NoSpaceOpenRound(1:3-1:4),NoSpaceOpenRound(1:4-1:5),LowerIdent(1:5-1:6),Newline(1:1-1:1),
CloseRound(2:1-2:2),Newline(1:1-1:1),
CloseRound(3:1-3:2),OpArrow(3:2-3:4),Int(3:4-3:6),EndOfFile(3:6-3:6),
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
