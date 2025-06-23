# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0b.0
0bu22
0u22
~~~
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }
Here is the problematic code:
**fuzz_crash_014.md:1:1:1:5:**
```roc
0b.0
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**UNEXPECTED TOKEN IN EXPRESSION**
The token **0u22** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_014.md:3:1:3:5:**
```roc
0u22
```


**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
MalformedNumberNoDigits(1:1-1:3),NoSpaceDotInt(1:3-1:5),Newline(1:1-1:1),
MalformedNumberNoDigits(2:1-2:6),Newline(1:1-1:1),
MalformedNumberBadSuffix(3:1-3:5),EndOfFile(3:5-3:5),
~~~
# PARSE
~~~clojure
(file (1:1-3:5)
	(malformed_header (1:1-1:5) "missing_header")
	(statements
		(malformed_expr (1:1-1:1) "expr_unexpected_token")
		(malformed_expr (1:1-1:1) "expr_unexpected_token")
		(malformed_expr (3:1-3:5) "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc



~~~
# CANONICALIZE
~~~clojure
(can_ir "empty")
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~