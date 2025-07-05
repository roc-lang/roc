# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0 b:S
.R
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_018.md:1:1:1:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_018.md:2:1:2:3
UNDECLARED TYPE - fuzz_crash_018.md:1:5:1:6
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_018.md:1:1:1:4:**
```roc
0 b:S
```
^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.R** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_018.md:2:1:2:3:**
```roc
.R
```
^^


**UNDECLARED TYPE**
The type ``S`` is not declared in this scope.

This type is referenced here:
**fuzz_crash_018.md:1:5:1:6:**
```roc
0 b:S
```
    ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
Int(1:1-1:2),LowerIdent(1:3-1:4),OpColon(1:4-1:5),UpperIdent(1:5-1:6),Newline(1:1-1:1),
DotUpperIdent(2:1-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(file @1.1-2.3
	(malformed-header @1.1-1.4 (tag "missing_header"))
	(statements
		(s-type-anno @1.1-1.1 (name "b")
			(ty @1.5-1.6 (name "S")))
		(e-malformed @2.1-2.3 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
b : S

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
