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
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_014.md:1:1:1:5
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_014.md:1:3:1:3
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_014.md:2:1:2:1
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_014.md:3:1:3:1
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_014.md:4:1:4:3
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_014.md:4:2:4:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_014.md:4:3:4:4
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
^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_014.md:1:3:1:3:**
```roc
0b.0
```
  


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_014.md:2:1:2:1:**
```roc
0bu22
```



**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_014.md:3:1:3:1:**
```roc
0u22
```



**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_014.md:4:1:4:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_014.md:4:2:4:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_014.md:4:3:4:4:**
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

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
MalformedNumberNoDigits(1:1-1:3),NoSpaceDotInt(1:3-1:5),Newline(1:1-1:1),
MalformedNumberNoDigits(2:1-2:6),Newline(1:1-1:1),
MalformedNumberBadSuffix(3:1-3:5),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(file @1.1-4.4
	(malformed-header @1.1-1.5 (tag "missing_header"))
	(statements
		(e-malformed @1.1-1.1 (reason "expr_unexpected_token"))
		(e-malformed @1.1-1.1 (reason "expr_unexpected_token"))
		(e-malformed @1.1-1.1 (reason "expr_unexpected_token"))
		(e-malformed @4.1-4.3 (reason "expr_unexpected_token"))
		(e-malformed @4.2-4.4 (reason "expr_unexpected_token"))
		(e-malformed @4.3-4.4 (reason "expr_unexpected_token"))))
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
