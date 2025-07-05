# META
~~~ini
description=multiline_apply_equals_multiline_apply
type=expr
~~~
# SOURCE
~~~roc

MT
 q
=g
 q
dbgT
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - multiline_apply_equals_multiline_apply.md:1:1:2:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **
MT** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multiline_apply_equals_multiline_apply.md:1:1:2:3:**
```roc

MT
```


# TOKENS
~~~zig
Newline(1:1-1:1),
UpperIdent(2:1-2:3),Newline(1:1-1:1),
LowerIdent(3:2-3:3),Newline(1:1-1:1),
OpAssign(4:1-4:2),LowerIdent(4:2-4:3),Newline(1:1-1:1),
LowerIdent(5:2-5:3),Newline(1:1-1:1),
LowerIdent(6:1-6:5),Newline(1:1-1:1),
MalformedUnknownToken(7:1-7:2),MalformedUnknownToken(7:2-7:3),MalformedUnknownToken(7:3-7:4),EndOfFile(7:4-7:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-2.3 (reason "expr_unexpected_token"))
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
