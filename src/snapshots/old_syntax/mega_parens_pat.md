# META
~~~ini
description=mega_parens_pat
type=expr
~~~
# SOURCE
~~~roc
1((0#
)f)((0#
)f):f
e
~~~
# EXPECTED
PARSE ERROR - mega_parens_pat.md:1:1:1:3
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_apply_close_round`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**mega_parens_pat.md:1:1:1:3:**
```roc
1((0#
```
^^


# TOKENS
~~~zig
Int(1:1-1:2),NoSpaceOpenRound(1:2-1:3),NoSpaceOpenRound(1:3-1:4),Int(1:4-1:5),Newline(1:6-1:6),
CloseRound(2:1-2:2),LowerIdent(2:2-2:3),CloseRound(2:3-2:4),NoSpaceOpenRound(2:4-2:5),NoSpaceOpenRound(2:5-2:6),Int(2:6-2:7),Newline(2:8-2:8),
CloseRound(3:1-3:2),LowerIdent(3:2-3:3),CloseRound(3:3-3:4),OpColon(3:4-3:5),LowerIdent(3:5-3:6),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-2.4 (reason "expected_expr_apply_close_round"))
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
