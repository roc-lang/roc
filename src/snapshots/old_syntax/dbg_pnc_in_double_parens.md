# META
~~~ini
description=dbg_pnc_in_double_parens
type=expr
~~~
# SOURCE
~~~roc
((dbg(r))
r)
~~~
# EXPECTED
expected_expr_close_round_or_comma - dbg_pnc_in_double_parens.md:2:2:2:3
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**dbg_pnc_in_double_parens.md:2:2:2:3:**
```roc
r)
```
 ^


# TOKENS
~~~zig
OpenRound(1:1-1:2),NoSpaceOpenRound(1:2-1:3),KwDbg(1:3-1:6),NoSpaceOpenRound(1:6-1:7),LowerIdent(1:7-1:8),CloseRound(1:8-1:9),CloseRound(1:9-1:10),Newline(1:1-1:1),
LowerIdent(2:1-2:2),CloseRound(2:2-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(e-malformed @2.2-2.3 (reason "expected_expr_close_round_or_comma"))
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
