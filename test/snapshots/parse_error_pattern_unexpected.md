# META
~~~ini
description=Pattern with unexpected token
type=expr
~~~
# SOURCE
~~~roc
match x
    + -> 1
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - parse_error_pattern_unexpected.md:2:7:2:9
PARSE ERROR - parse_error_pattern_unexpected.md:2:10:2:11
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **->** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**parse_error_pattern_unexpected.md:2:7:2:9:**
```roc
    + -> 1
```
      ^^


**PARSE ERROR**
A parsing error occurred: `expected_open_curly_after_match`
This is an unexpected parsing error. Please check your syntax.

**parse_error_pattern_unexpected.md:2:10:2:11:**
```roc
    + -> 1
```
         ^


# TOKENS
~~~zig
KwMatch,LowerIdent,
OpPlus,OpArrow,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-malformed (reason "expected_open_curly_after_match"))
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
