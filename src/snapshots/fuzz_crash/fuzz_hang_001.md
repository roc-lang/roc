# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0 (
~~~
~~~
# EXPECTED
MISSING HEADER - fuzz_hang_001.md:1:1:1:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_hang_001.md:2:1:2:3
PARSE ERROR - fuzz_hang_001.md:2:4:2:4
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_hang_001.md:1:1:1:4:**
```roc
0 (
```
^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_hang_001.md:2:1:2:3:**
```roc
~~~
```
^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_hang_001.md:2:4:2:4:**
```roc
~~~
```
   


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
Int(1:1-1:2),OpenRound(1:3-1:4),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(file @1.1-2.4
	(malformed-header @1.1-1.4 (tag "missing_header"))
	(statements
		(e-malformed @2.4-2.4 (reason "expected_expr_close_round_or_comma"))))
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
