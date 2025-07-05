# META
~~~ini
description=multiline_str_interpolation_records
type=expr
~~~
# SOURCE
~~~roc
"""${{
}i}
${{
}i}"""
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **"""** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multiline_str_interpolation_records.md:1:1:1:4:**
```roc
"""${{
```
^^^


# TOKENS
~~~zig
MultilineStringStart(1:1-1:4),StringPart(1:4-1:4),OpenStringInterpolation(1:4-1:6),OpenCurly(1:6-1:7),Newline(1:1-1:1),
CloseCurly(2:1-2:2),LowerIdent(2:2-2:3),CloseStringInterpolation(2:3-2:4),StringPart(2:4-2:4),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),OpenCurly(3:2-3:3),OpenCurly(3:3-3:4),Newline(1:1-1:1),
CloseCurly(4:1-4:2),LowerIdent(4:2-4:3),CloseCurly(4:3-4:4),MultilineStringStart(4:4-4:7),StringPart(4:7-4:7),EndOfFile(4:7-4:7),
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
