# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module P]F
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_011.md:1:8:1:9
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_011.md:1:9:1:10
PARSE ERROR - fuzz_crash_011.md:1:11:1:11
INVALID STATEMENT - fuzz_crash_011.md:1:9:1:10
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `header_expected_open_square`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_011.md:1:8:1:9:**
```roc
module P]F
```
       ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_011.md:1:9:1:10:**
```roc
module P]F
```
        ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

Here is the problematic code:
**fuzz_crash_011.md:1:11:1:11:**
```roc
module P]F
```
          


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_011.md:1:9:1:10:**
```roc
module P]F
```
        ^


# TOKENS
~~~zig
KwModule(1:1-1:7),UpperIdent(1:8-1:9),CloseSquare(1:9-1:10),UpperIdent(1:10-1:11),EndOfFile(1:11-1:11),
~~~
# PARSE
~~~clojure
(file @1.1-1.11
	(malformed-header @1.8-1.9 (tag "header_expected_open_square"))
	(statements
		(e-malformed @1.9-1.10 (reason "expr_unexpected_token"))
		(s-malformed @1.10-1.11 (tag "expected_colon_after_type_annotation"))))
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
