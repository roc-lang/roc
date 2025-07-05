# META
~~~ini
description=basic_docs
type=expr
~~~
# SOURCE
~~~roc
## first line of docs
##     second line
##  third line
## fourth line
##
## sixth line after doc new line
x = 5

42
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - basic_docs.md:1:2:2:19
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **# first line of docs
##     second line** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**basic_docs.md:1:2:2:19:**
```roc
## first line of docs
##     second line
```


# TOKENS
~~~zig
Newline(1:2-1:22),
Newline(2:2-2:19),
Newline(3:2-3:15),
Newline(4:2-4:15),
Newline(5:2-5:3),
Newline(6:2-6:33),
LowerIdent(7:1-7:2),OpAssign(7:3-7:4),Int(7:5-7:6),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(9:1-9:3),Newline(1:1-1:1),
MalformedUnknownToken(10:1-10:2),MalformedUnknownToken(10:2-10:3),MalformedUnknownToken(10:3-10:4),EndOfFile(10:4-10:4),
~~~
# PARSE
~~~clojure
(e-malformed @1.2-2.19 (reason "expr_unexpected_token"))
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
