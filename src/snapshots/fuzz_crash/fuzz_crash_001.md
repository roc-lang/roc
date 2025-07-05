# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
mo|%
~~~
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_001.md:1:1:1:4
UNEXPECTED TOKEN IN PATTERN - fuzz_crash_001.md:1:4:1:4
PARSE ERROR - fuzz_crash_001.md:1:1:2:2
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_001.md:2:1:2:3
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_001.md:2:2:2:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_001.md:2:3:2:4
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_001.md:1:1:1:4:**
```roc
mo|%
```
^^^


**UNEXPECTED TOKEN IN PATTERN**
The token  is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

Here is the problematic code:
**fuzz_crash_001.md:1:4:1:4:**
```roc
mo|%
```
   


**PARSE ERROR**
A parsing error occurred: `expected_expr_bar`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_001.md:1:1:2:2:**
```roc
mo|%
~~~
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_001.md:2:1:2:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_001.md:2:2:2:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_001.md:2:3:2:4:**
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

# TOKENS
~~~zig
LowerIdent(1:1-1:3),OpBar(1:3-1:4),OpPercent(1:4-1:5),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(file @1.1-2.4
	(malformed-header @1.1-1.4 (tag "missing_header"))
	(statements
		(e-malformed @1.1-2.2 (reason "expected_expr_bar"))
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
