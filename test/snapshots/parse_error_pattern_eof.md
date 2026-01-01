# META
~~~ini
description=Pattern that ends unexpectedly
type=expr
~~~
# SOURCE
~~~roc
match x {
    [a,
~~~
# EXPECTED
UNEXPECTED TOKEN IN PATTERN - parse_error_pattern_eof.md:2:5:2:6
PARSE ERROR - parse_error_pattern_eof.md:3:1:3:1
UNEXPECTED TOKEN IN EXPRESSION - parse_error_pattern_eof.md:3:1:3:1
PARSE ERROR - parse_error_pattern_eof.md:3:1:3:1
# PROBLEMS
**UNEXPECTED TOKEN IN PATTERN**
The token **[** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**parse_error_pattern_eof.md:2:5:2:6:**
```roc
    [a,
```
    ^


**PARSE ERROR**
A parsing error occurred: `match_branch_missing_arrow`
This is an unexpected parsing error. Please check your syntax.

**parse_error_pattern_eof.md:3:1:3:1:**
```roc

```
^


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**parse_error_pattern_eof.md:3:1:3:1:**
```roc

```
^


**PARSE ERROR**
A parsing error occurred: `expected_close_curly_at_end_of_match`
This is an unexpected parsing error. Please check your syntax.

**parse_error_pattern_eof.md:3:1:3:1:**
```roc

```
^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,Comma,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "expected_close_curly_at_end_of_match"))
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
