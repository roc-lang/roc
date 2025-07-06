# META
~~~ini
description=unindented_if_in_closure
type=expr
~~~
# SOURCE
~~~roc
\A->if!s!then
f
else-9
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - unindented_if_in_closure.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\A** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**unindented_if_in_closure.md:1:1:1:3:**
```roc
\A->if!s!then
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),UpperIdent(1:2-1:3),OpArrow(1:3-1:5),LowerIdent(1:5-1:14),Newline(1:1-1:1),
LowerIdent(2:1-2:2),Newline(1:1-1:1),
KwElse(3:1-3:5),OpBinaryMinus(3:5-3:6),Int(3:6-3:7),EndOfFile(3:7-3:7),
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
