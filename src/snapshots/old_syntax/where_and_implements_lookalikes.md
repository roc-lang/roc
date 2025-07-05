# META
~~~ini
description=where_and_implements_lookalikes
type=expr
~~~
# SOURCE
~~~roc
(i:a#
  wherew implementsI
e)
~~~
# EXPECTED
expected_expr_close_round_or_comma - where_and_implements_lookalikes.md:3:2:3:3
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_and_implements_lookalikes.md:3:2:3:3:**
```roc
e)
```
 ^


# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:2-1:3),OpColon(1:3-1:4),LowerIdent(1:4-1:5),Newline(1:6-1:6),
LowerIdent(2:3-2:9),LowerIdent(2:10-2:21),Newline(1:1-1:1),
LowerIdent(3:1-3:2),CloseRound(3:2-3:3),EndOfFile(3:3-3:3),
~~~
# PARSE
~~~clojure
(e-malformed @3.2-3.3 (reason "expected_expr_close_round_or_comma"))
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
