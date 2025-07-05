# META
~~~ini
description=closure_complex_pattern_indent_issue
type=expr
~~~
# SOURCE
~~~roc
\I{p?Y
 Y}[
]->
 K#(
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - closure_complex_pattern_indent_issue.md:1:1:1:3
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\I** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**closure_complex_pattern_indent_issue.md:1:1:1:3:**
```roc
\I{p?Y
```
^^


# TOKENS
~~~zig
OpBackslash(1:1-1:2),UpperIdent(1:2-1:3),OpenCurly(1:3-1:4),LowerIdent(1:4-1:5),NoSpaceOpQuestion(1:5-1:6),UpperIdent(1:6-1:7),Newline(1:1-1:1),
UpperIdent(2:2-2:3),CloseCurly(2:3-2:4),OpenSquare(2:4-2:5),Newline(1:1-1:1),
CloseSquare(3:1-3:2),OpArrow(3:2-3:4),Newline(1:1-1:1),
UpperIdent(4:2-4:3),EndOfFile(4:5-4:5),
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
