# META
~~~ini
description=return_parens_comments
type=expr
~~~
# SOURCE
~~~roc
return(3#
#
)
Z
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **return(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**return_parens_comments.md:1:1:1:8:**
```roc
return(3#
```
^^^^^^^


# TOKENS
~~~zig
KwReturn(1:1-1:7),NoSpaceOpenRound(1:7-1:8),Int(1:8-1:9),Newline(1:10-1:10),
Newline(2:2-2:2),
CloseRound(3:1-3:2),Newline(1:1-1:1),
UpperIdent(4:1-4:2),EndOfFile(4:2-4:2),
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
