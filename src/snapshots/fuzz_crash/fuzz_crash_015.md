# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0o0.0
0_0
0u8.0
0_
~~~
~~~
# EXPECTED
LEADING ZERO - fuzz_crash_015.md:1:1:1:6
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_015.md:1:4:1:4
PARSE ERROR - fuzz_crash_015.md:3:4:3:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_015.md:5:1:5:3
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_015.md:5:2:5:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_015.md:5:3:5:4
# PROBLEMS
**LEADING ZERO**
Numbers cannot have leading zeros.

**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_015.md:1:1:1:6:**
```roc
0o0.0
```
^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_015.md:1:4:1:4:**
```roc
0o0.0
```
   


**PARSE ERROR**
A parsing error occurred: `expr_no_space_dot_int`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_015.md:3:4:3:4:**
```roc
0u8.0
```
   


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_015.md:5:1:5:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_015.md:5:2:5:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_015.md:5:3:5:4:**
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

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
Int(1:1-1:4),NoSpaceDotInt(1:4-1:6),Newline(1:1-1:1),
Int(2:1-2:4),Newline(1:1-1:1),
Int(3:1-3:4),NoSpaceDotInt(3:4-3:6),Newline(1:1-1:1),
Int(4:1-4:3),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(file @1.1-5.4
	(malformed-header @1.1-1.6 (tag "missing_header"))
	(statements
		(e-malformed @1.1-1.1 (reason "expr_unexpected_token"))
		(e-int @2.1-2.4 (raw "0_0"))
		(e-malformed @1.1-1.1 (reason "expr_no_space_dot_int"))
		(e-int @4.1-4.3 (raw "0_"))
		(e-malformed @5.1-5.3 (reason "expr_unexpected_token"))
		(e-malformed @5.2-5.4 (reason "expr_unexpected_token"))
		(e-malformed @5.3-5.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc

0_0

0_

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
