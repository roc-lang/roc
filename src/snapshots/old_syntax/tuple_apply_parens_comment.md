# META
~~~ini
description=tuple_apply_parens_comment
type=expr
~~~
# SOURCE
~~~roc
((L
L)L,L)#\
~~~
# EXPECTED
expected_expr_close_round_or_comma - tuple_apply_parens_comment.md:2:2:2:4
expected_expr_close_round_or_comma - tuple_apply_parens_comment.md:2:6:2:9
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**tuple_apply_parens_comment.md:2:2:2:4:**
```roc
L)L,L)#\
```
 ^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**tuple_apply_parens_comment.md:2:6:2:9:**
```roc
L)L,L)#\
```
     ^^^


# TOKENS
~~~zig
OpenRound(1:1-1:2),NoSpaceOpenRound(1:2-1:3),UpperIdent(1:3-1:4),Newline(1:1-1:1),
UpperIdent(2:1-2:2),CloseRound(2:2-2:3),UpperIdent(2:3-2:4),Comma(2:4-2:5),UpperIdent(2:5-2:6),CloseRound(2:6-2:7),EndOfFile(2:9-2:9),
~~~
# PARSE
~~~clojure
(e-malformed @2.6-2.9 (reason "expected_expr_close_round_or_comma"))
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
