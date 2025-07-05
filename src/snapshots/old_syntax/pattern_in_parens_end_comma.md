# META
~~~ini
description=pattern_in_parens_end_comma fail
type=expr
~~~
# SOURCE
~~~roc
\( a,
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - pattern_in_parens_end_comma.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**pattern_in_parens_end_comma.md:1:1:1:3:**
```roc
\( a,
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),NoSpaceOpenRound(1:2-1:3),LowerIdent(1:4-1:5),Comma(1:5-1:6),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.3 (reason "expr_unexpected_token"))
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
