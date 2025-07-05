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
**fuzz_crash_002.md:1:1:1:6:**
```roc
modu:;::::::::::::::le[%
```
^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:;** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_002.md:1:5:1:7:**
```roc
modu:;::::::::::::::le[%
```
    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **;:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_002.md:1:6:1:8:**
```roc
modu:;::::::::::::::le[%
```
     ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **::** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_002.md:1:7:1:9:**
```roc
modu:;::::::::::::::le[%
```
      ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **::** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_002.md:1:8:1:10:**
```roc
modu:;::::::::::::::le[%
```
       ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **::** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_002.md:1:9:1:11:**
```roc
modu:;::::::::::::::le[%
```
        ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **::** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_002.md:1:10:1:12:**
```roc
modu:;::::::::::::::le[%
```
         ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **::** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_002.md:1:11:1:13:**
```roc
modu:;::::::::::::::le[%
```
          ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **::** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_002.md:1:12:1:14:**
```roc
modu:;::::::::::::::le[%
```
           ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **::** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_002.md:1:13:1:15:**
```roc
modu:;::::::::::::::le[%
```
            ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **::** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_002.md:1:14:1:16:**
```roc
modu:;::::::::::::::le[%
```
             ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **::** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_002.md:1:15:1:17:**
```roc
modu:;::::::::::::::le[%
```
              ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **::** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_002.md:1:16:1:18:**
```roc
modu:;::::::::::::::le[%
```
               ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **::** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_002.md:1:17:1:19:**
```roc
modu:;::::::::::::::le[%
```
                ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **::** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_002.md:1:18:1:20:**
```roc
modu:;::::::::::::::le[%
```
                 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **::** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_002.md:1:19:1:21:**
```roc
modu:;::::::::::::::le[%
```
                  ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:le** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_002.md:1:20:1:23:**
```roc
modu:;::::::::::::::le[%
```
                   ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **%** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_002.md:1:24:1:25:**
```roc
modu:;::::::::::::::le[%
```
                       ^


**LIST NOT CLOSED**
This list is missing a closing bracket or has a syntax error.
Lists must be closed with **]** and list items must be separated by commas.
For example:     [1, 2, 3]

Here is the problematic code:
**fuzz_crash_002.md:1:25:1:25:**
```roc
modu:;::::::::::::::le[%
```
                        


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpColon(1:5-1:6),MalformedUnknownToken(1:6-1:7),OpColon(1:7-1:8),OpColon(1:8-1:9),OpColon(1:9-1:10),OpColon(1:10-1:11),OpColon(1:11-1:12),OpColon(1:12-1:13),OpColon(1:13-1:14),OpColon(1:14-1:15),OpColon(1:15-1:16),OpColon(1:16-1:17),OpColon(1:17-1:18),OpColon(1:18-1:19),OpColon(1:19-1:20),OpColon(1:20-1:21),LowerIdent(1:21-1:23),OpenSquare(1:23-1:24),OpPercent(1:24-1:25),EndOfFile(1:25-1:25),
~~~
# PARSE
~~~clojure
(file @1.1-1.25
	(malformed-header @1.1-1.6 (tag "missing_header"))
	(statements
		(e-malformed @1.5-1.7 (reason "expr_unexpected_token"))
		(e-malformed @1.6-1.8 (reason "expr_unexpected_token"))
		(e-malformed @1.7-1.9 (reason "expr_unexpected_token"))
		(e-malformed @1.8-1.10 (reason "expr_unexpected_token"))
		(e-malformed @1.9-1.11 (reason "expr_unexpected_token"))
		(e-malformed @1.10-1.12 (reason "expr_unexpected_token"))
		(e-malformed @1.11-1.13 (reason "expr_unexpected_token"))
		(e-malformed @1.12-1.14 (reason "expr_unexpected_token"))
		(e-malformed @1.13-1.15 (reason "expr_unexpected_token"))
		(e-malformed @1.14-1.16 (reason "expr_unexpected_token"))
		(e-malformed @1.15-1.17 (reason "expr_unexpected_token"))
		(e-malformed @1.16-1.18 (reason "expr_unexpected_token"))
		(e-malformed @1.17-1.19 (reason "expr_unexpected_token"))
		(e-malformed @1.18-1.20 (reason "expr_unexpected_token"))
		(e-malformed @1.19-1.21 (reason "expr_unexpected_token"))
		(e-malformed @1.20-1.23 (reason "expr_unexpected_token"))
		(e-ident @1.21-1.23 (qaul "") (raw "le"))
		(e-malformed @1.25-1.25 (reason "expected_expr_close_square_or_comma"))))
~~~
# FORMATTED
~~~roc
le
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
