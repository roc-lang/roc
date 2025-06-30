# META
~~~ini
description=tuples_parens_comments
type=expr
~~~
# SOURCE
~~~roc
(i,#

(
#
(EsE))
ui)
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**tuples_parens_comments.md:6:3:6:4:**
```roc
ui)
```
  ^


# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:2-1:3),Comma(1:3-1:4),Newline(1:5-1:5),
Newline(1:1-1:1),
OpenRound(3:1-3:2),Newline(1:1-1:1),
Newline(4:2-4:2),
OpenRound(5:1-5:2),UpperIdent(5:2-5:5),CloseRound(5:5-5:6),CloseRound(5:6-5:7),Newline(1:1-1:1),
LowerIdent(6:1-6:3),CloseRound(6:3-6:4),EndOfFile(6:4-6:4),
~~~
# PARSE
~~~clojure
(e-malformed @6.3-6.4 (reason "expected_expr_close_round_or_comma"))
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
