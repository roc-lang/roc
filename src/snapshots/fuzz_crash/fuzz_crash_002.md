# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
modu:;::::::::::::::le[%
~~~
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }
Here is the problematic code:
**fuzz_crash_002.md:1-0:1:**
```roc
modu:;::::::::::::::le[%
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_002.md:1-4:1:**
```roc
modu:;::::::::::::::le[%
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_002.md:1-5:1:**
```roc
modu:;::::::::::::::le[%
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_002.md:1-6:1:**
```roc
modu:;::::::::::::::le[%
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_002.md:1-7:1:**
```roc
modu:;::::::::::::::le[%
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_002.md:1-8:1:**
```roc
modu:;::::::::::::::le[%
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_002.md:1-9:1:**
```roc
modu:;::::::::::::::le[%
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_002.md:1-10:1:**
```roc
modu:;::::::::::::::le[%
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_002.md:1-11:1:**
```roc
modu:;::::::::::::::le[%
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_002.md:1-12:1:**
```roc
modu:;::::::::::::::le[%
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_002.md:1-13:1:**
```roc
modu:;::::::::::::::le[%
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_002.md:1-14:1:**
```roc
modu:;::::::::::::::le[%
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_002.md:1-15:1:**
```roc
modu:;::::::::::::::le[%
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_002.md:1-16:1:**
```roc
modu:;::::::::::::::le[%
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_002.md:1-17:1:**
```roc
modu:;::::::::::::::le[%
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_002.md:1-18:1:**
```roc
modu:;::::::::::::::le[%
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_002.md:1-19:1:**
```roc
modu:;::::::::::::::le[%
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**fuzz_crash_002.md:1-23:1:**
```roc
modu:;::::::::::::::le[%
```


**LIST NOT CLOSED**
This list is missing a closing bracket or has a syntax error.
Lists must be closed with **]** and list items must be separated by commas.
For example:     [1, 2, 3]

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

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
LowerIdent(1:1-1:5),OpColon(1:5-1:6),MalformedUnknownToken(1:6-1:7),OpColon(1:7-1:8),OpColon(1:8-1:9),OpColon(1:9-1:10),OpColon(1:10-1:11),OpColon(1:11-1:12),OpColon(1:12-1:13),OpColon(1:13-1:14),OpColon(1:14-1:15),OpColon(1:15-1:16),OpColon(1:16-1:17),OpColon(1:17-1:18),OpColon(1:18-1:19),OpColon(1:19-1:20),OpColon(1:20-1:21),LowerIdent(1:21-1:23),OpenSquare(1:23-1:24),OpPercent(1:24-1:25),EndOfFile(1:25-1:25),
~~~
# PARSE
~~~clojure
(file (1:1-1:25)
	(malformed_header (1:1-1:5) "missing_header")
	(statements
		(malformed_expr (1:5-1:6) "expr_unexpected_token")
		(malformed_expr (1:6-1:7) "expr_unexpected_token")
		(malformed_expr (1:7-1:8) "expr_unexpected_token")
		(malformed_expr (1:8-1:9) "expr_unexpected_token")
		(malformed_expr (1:9-1:10) "expr_unexpected_token")
		(malformed_expr (1:10-1:11) "expr_unexpected_token")
		(malformed_expr (1:11-1:12) "expr_unexpected_token")
		(malformed_expr (1:12-1:13) "expr_unexpected_token")
		(malformed_expr (1:13-1:14) "expr_unexpected_token")
		(malformed_expr (1:14-1:15) "expr_unexpected_token")
		(malformed_expr (1:15-1:16) "expr_unexpected_token")
		(malformed_expr (1:16-1:17) "expr_unexpected_token")
		(malformed_expr (1:17-1:18) "expr_unexpected_token")
		(malformed_expr (1:18-1:19) "expr_unexpected_token")
		(malformed_expr (1:19-1:20) "expr_unexpected_token")
		(malformed_expr (1:20-1:21) "expr_unexpected_token")
		(ident (1:21-1:23) "" "le")
		(malformed_expr (1:25-1:25) "expected_expr_close_square_or_comma")))
~~~
# FORMATTED
~~~roc
le
~~~
# CANONICALIZE
~~~clojure
(can_ir "empty")
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~