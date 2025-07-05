# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module P]F
~~~
~~~
# EXPECTED
OVER CLOSED BRACE - fuzz_crash_011.md:1:8:1:11
PARSE ERROR - fuzz_crash_011.md:1:10:1:10
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_011.md:2:2:2:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_011.md:2:3:2:4
# PROBLEMS
**OVER CLOSED BRACE**
There are too many closing braces here.

**PARSE ERROR**
A parsing error occurred: `header_expected_open_square`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_011.md:1:8:1:11:**
```roc
module P]F
```
       ^^^


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
**fuzz_crash_011.md:1:10:1:10:**
```roc
module P]F
```
         


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_011.md:2:2:2:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_011.md:2:3:2:4:**
```roc
~~~
```
  ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwModule(1:1-1:7),UpperIdent(1:8-1:9),UpperIdent(1:10-1:11),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(file @1.1-2.4
	(malformed-header @1.8-1.11 (tag "header_expected_open_square"))
	(statements
		(s-malformed @1.10-2.3 (tag "expected_colon_after_type_annotation"))
		(e-malformed @2.2-2.4 (reason "expr_unexpected_token"))
		(e-malformed @2.3-2.4 (reason "expr_unexpected_token"))))
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
