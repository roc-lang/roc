# META
~~~ini
description=alias_or_opaque_fail fail
type=expr
~~~
# SOURCE
~~~roc
(@,B
.e:
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **@,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**alias_or_opaque_fail.md:1:2:1:4:**
```roc
(@,B
```
 ^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**alias_or_opaque_fail.md:2:4:2:4:**
```roc
.e:
```
   


# TOKENS
~~~zig
OpenRound(1:1-1:2),MalformedOpaqueNameWithoutName(1:2-1:3),Comma(1:3-1:4),UpperIdent(1:4-1:5),Newline(1:1-1:1),
DotLowerIdent(2:1-2:3),OpColon(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-malformed @2.4-2.4 (reason "expected_expr_close_round_or_comma"))
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
