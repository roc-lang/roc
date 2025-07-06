# META
~~~ini
description=multiline_str_crazyness
type=expr
~~~
# SOURCE
~~~roc
"""""""${i""""""}"
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - multiline_str_crazyness.md:1:1:1:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **"""** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multiline_str_crazyness.md:1:1:1:4:**
```roc
"""""""${i""""""}"
```
^^^


# TOKENS
~~~zig
MultilineStringStart(1:1-1:4),StringPart(1:4-1:4),MultilineStringEnd(1:4-1:7),StringStart(1:7-1:8),StringPart(1:8-1:8),OpenStringInterpolation(1:8-1:10),LowerIdent(1:10-1:11),MultilineStringStart(1:11-1:14),StringPart(1:14-1:14),MultilineStringEnd(1:14-1:17),CloseStringInterpolation(1:17-1:18),StringPart(1:18-1:18),StringEnd(1:18-1:19),EndOfFile(1:19-1:19),
~~~
# PARSE
~~~clojure
(e-malformed @1.1-1.4 (reason "expr_unexpected_token"))
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
