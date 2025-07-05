# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
ff8.8.d
~~~
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_007.md:1:1:1:6
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_007.md:1:4:1:8
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_007.md:1:6:1:6
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_007.md:2:1:2:3
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_007.md:2:2:2:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_007.md:2:3:2:4
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_007.md:1:1:1:6:**
```roc
ff8.8.d
```
^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.8.d** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_007.md:1:4:1:8:**
```roc
ff8.8.d
```
   ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_007.md:1:6:1:6:**
```roc
ff8.8.d
```
     


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_007.md:2:1:2:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_007.md:2:2:2:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_007.md:2:3:2:4:**
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
LowerIdent(1:1-1:4),NoSpaceDotInt(1:4-1:6),NoSpaceDotLowerIdent(1:6-1:8),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(file @1.1-2.4
	(malformed-header @1.1-1.6 (tag "missing_header"))
	(statements
		(e-malformed @1.4-1.8 (reason "expr_unexpected_token"))
		(e-malformed @1.1-1.1 (reason "expr_unexpected_token"))
		(e-malformed @2.1-2.3 (reason "expr_unexpected_token"))
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
