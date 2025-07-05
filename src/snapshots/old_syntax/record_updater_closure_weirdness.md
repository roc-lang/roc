# META
~~~ini
description=record_updater_closure_weirdness
type=expr
~~~
# SOURCE
~~~roc
&rm?\L2->t
+c
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - record_updater_closure_weirdness.md:1:1:1:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **&rm** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_updater_closure_weirdness.md:1:1:1:4:**
```roc
&rm?\L2->t
```
^^^


# TOKENS
~~~zig
OpAmpersand(1:1-1:2),LowerIdent(1:2-1:4),NoSpaceOpQuestion(1:4-1:5),OpBackslash(1:5-1:6),UpperIdent(1:6-1:8),OpArrow(1:8-1:10),LowerIdent(1:10-1:11),Newline(1:1-1:1),
OpPlus(2:1-2:2),LowerIdent(2:2-2:3),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.4 (reason "expr_unexpected_token"))
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
