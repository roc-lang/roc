# META
~~~ini
description=record_destructure_def
type=expr
~~~
# SOURCE
~~~roc
# leading comment
{ x, y } = 5
y = 6

42
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token ** leading comment
{** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_destructure_def.md:1:2:2:2:**
```roc
# leading comment
{ x, y } = 5
```


# TOKENS
~~~zig
Newline(1:2-1:18),
OpenCurly(2:1-2:2),LowerIdent(2:3-2:4),Comma(2:4-2:5),LowerIdent(2:6-2:7),CloseCurly(2:8-2:9),OpAssign(2:10-2:11),Int(2:12-2:13),Newline(1:1-1:1),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),Int(3:5-3:6),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(5:1-5:3),EndOfFile(5:3-5:3),
~~~
# PARSE
~~~clojure
(e-malformed @1.2-2.2 (reason "expr_unexpected_token"))
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
