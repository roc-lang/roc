# META
~~~ini
description=record_literal_field_bang
type=expr
~~~
# SOURCE
~~~roc
{
    answer: 42,
    launchTheNukes!: \{} -> boom
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\{** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_literal_field_bang.md:3:22:3:24:**
```roc
    launchTheNukes!: \{} -> boom
```
                     ^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**record_literal_field_bang.md:3:23:3:25:**
```roc
    launchTheNukes!: \{} -> boom
```
                      ^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:5-2:11),OpColon(2:11-2:12),Int(2:13-2:15),Comma(2:15-2:16),Newline(1:1-1:1),
LowerIdent(3:5-3:20),OpColon(3:20-3:21),OpBackslash(3:22-3:23),OpenCurly(3:23-3:24),CloseCurly(3:24-3:25),OpArrow(3:26-3:28),LowerIdent(3:29-3:33),Newline(1:1-1:1),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-malformed @3.23-3.25 (reason "expected_expr_close_curly_or_comma"))
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
