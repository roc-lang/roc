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
UNCLOSED SINGLE QUOTE - fuzz_crash_031.md:1:1:1:7
INVALID STATEMENT - fuzz_crash_031.md:1:6:4:6
INVALID STATEMENT - fuzz_crash_031.md:4:1:4:8
# PROBLEMS
**UNCLOSED SINGLE QUOTE**
This character literal is missing a closing single quote.

**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_031.md:1:1:1:7:**
```roc
mule []
```
^^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_031.md:1:6:4:6:**
```roc
mule []

#el
vavar t= '
```


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_031.md:4:1:4:8:**
```roc
vavar t= '
```
^^^^^^^


**INVALID SCALAR**
I am part way through parsing this scalar literal (character literal), but it appears to be invalid.

# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpenSquare(1:6-1:7),CloseSquare(1:7-1:8),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:4),
LowerIdent(4:1-4:6),LowerIdent(4:7-4:8),OpAssign(4:8-4:9),SingleQuote(4:10-4:11),EndOfFile(4:11-4:11),
~~~
# PARSE
~~~clojure
(file @1.1-4.11
	(malformed-header @1.1-1.7 (tag "missing_header"))
	(statements
		(e-list @1.6-1.8)
		(e-ident @4.1-4.6 (raw "vavar"))
		(s-decl @4.7-4.11
			(p-ident @4.7-4.8 (raw "t"))
			(e-single-quote @4.10-4.11 (raw "'")))))
~~~
# FORMATTED
~~~roc
[]

# el
vavart = '
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.7-4.8 (ident "t"))
		(e-runtime-error (tag "invalid_single_quote"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.7-4.11 (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
