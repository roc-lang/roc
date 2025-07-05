# META
~~~ini
description=comment_after_whitespace_apply_arg_inside_pnc_apply
type=expr
~~~
# SOURCE
~~~roc
PP(P@P(P
P(PPP(P
PPP
)
)
PP(mport#<|"P
)
)
)
~~~
# EXPECTED
PARSE ERROR - comment_after_whitespace_apply_arg_inside_pnc_apply.md:1:1:1:4
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_apply_close_round`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**comment_after_whitespace_apply_arg_inside_pnc_apply.md:1:1:1:4:**
```roc
PP(P@P(P
```
^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:3),NoSpaceOpenRound(1:3-1:4),UpperIdent(1:4-1:5),OpaqueName(1:5-1:7),NoSpaceOpenRound(1:7-1:8),UpperIdent(1:8-1:9),Newline(1:1-1:1),
UpperIdent(2:1-2:2),NoSpaceOpenRound(2:2-2:3),UpperIdent(2:3-2:6),NoSpaceOpenRound(2:6-2:7),UpperIdent(2:7-2:8),Newline(1:1-1:1),
UpperIdent(3:1-3:4),Newline(1:1-1:1),
CloseRound(4:1-4:2),Newline(1:1-1:1),
CloseRound(5:1-5:2),Newline(1:1-1:1),
UpperIdent(6:1-6:3),NoSpaceOpenRound(6:3-6:4),LowerIdent(6:4-6:9),Newline(6:10-6:14),
CloseRound(7:1-7:2),Newline(1:1-1:1),
CloseRound(8:1-8:2),Newline(1:1-1:1),
CloseRound(9:1-9:2),EndOfFile(9:2-9:2),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.8 (reason "expected_expr_apply_close_round"))
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
