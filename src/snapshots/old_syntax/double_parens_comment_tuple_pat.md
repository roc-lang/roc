# META
~~~ini
description=double_parens_comment_tuple_pat
type=expr
~~~
# SOURCE
~~~roc
((0#
)e,(0#
)p):f
t#
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**double_parens_comment_tuple_pat.md:3:1:3:3:**
```roc
)p):f
```
^^


# TOKENS
~~~zig
OpenRound(1:1-1:2),NoSpaceOpenRound(1:2-1:3),Int(1:3-1:4),Newline(1:5-1:5),
CloseRound(2:1-2:2),LowerIdent(2:2-2:3),Comma(2:3-2:4),NoSpaceOpenRound(2:4-2:5),Int(2:5-2:6),Newline(2:7-2:7),
CloseRound(3:1-3:2),LowerIdent(3:2-3:3),CloseRound(3:3-3:4),OpColon(3:4-3:5),LowerIdent(3:5-3:6),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:3-4:3),
~~~
# PARSE
~~~clojure
(e-malformed @3.1-3.3 (reason "expected_expr_close_round_or_comma"))
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
