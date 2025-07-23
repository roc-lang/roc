# META
~~~ini
description=fuzz crash, unterminated single quote
type=file
~~~
# SOURCE
~~~roc
mule []

#el
vavar t= '
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_031.md:1:1:1:5
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_031.md:4:10:4:11
COMPILER DIAGNOSTIC - fuzz_crash_031.md:0:0:0:0
COMPILER DIAGNOSTIC - fuzz_crash_031.md:0:0:0:0
COMPILER DIAGNOSTIC - fuzz_crash_031.md:0:0:0:0
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_031.md:1:1:1:5:**
```roc
mule []
```
^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_031.md:4:10:4:11:**
```roc
vavar t= '
```
         ^


**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'invalid_top_level_statement' is not yet handled in report generation.
**fuzz_crash_031.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'invalid_top_level_statement' is not yet handled in report generation.
**fuzz_crash_031.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'expr_not_canonicalized' is not yet handled in report generation.
**fuzz_crash_031.md:0:0:0:0**

# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpenSquare(1:6-1:7),CloseSquare(1:7-1:8),
LowerIdent(4:1-4:6),LowerIdent(4:7-4:8),OpAssign(4:8-4:9),MalformedSingleQuoteUnclosed(4:10-4:11),EndOfFile(4:11-4:11),
~~~
# PARSE
~~~clojure
(file @1.1-4.11
	(malformed-header @1.1-1.5 (tag "missing_header"))
	(statements
		(e-list @1.6-1.8)
		(e-ident @4.1-4.6 (raw "vavar"))
		(s-decl @4.7-4.11
			(p-ident @4.7-4.8 (raw "t"))
			(e-malformed @4.10-4.11 (reason "expr_unexpected_token")))))
~~~
# FORMATTED
~~~roc
[]

# el
vavar
t = 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(def
		(pattern
			(p-assign @4.7-4.8 (ident "t")))
		(expr
			(e-runtime-error (tag "expr_not_canonicalized")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.7-4.8 (type "Error")))
	(expressions
		(expr @1.1-1.1 (type "Error"))))
~~~
