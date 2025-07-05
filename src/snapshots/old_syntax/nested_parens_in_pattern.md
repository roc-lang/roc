# META
~~~ini
description=nested_parens_in_pattern
type=expr
~~~
# SOURCE
~~~roc
((J)x):i
i
~~~
# EXPECTED
PARSE ERROR - nested_parens_in_pattern.md:1:6:1:8
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**nested_parens_in_pattern.md:1:6:1:8:**
```roc
((J)x):i
```
     ^^


# TOKENS
~~~zig
OpenRound(1:1-1:2),NoSpaceOpenRound(1:2-1:3),UpperIdent(1:3-1:4),CloseRound(1:4-1:5),LowerIdent(1:5-1:6),CloseRound(1:6-1:7),OpColon(1:7-1:8),LowerIdent(1:8-1:9),Newline(1:1-1:1),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-malformed @1.6-1.8 (reason "expected_expr_close_round_or_comma"))
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
