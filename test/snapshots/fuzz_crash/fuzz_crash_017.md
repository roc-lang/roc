# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
me = "luc"
foo = "hello ${namF
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_017.md:1:1:1:3
PARSE ERROR - fuzz_crash_017.md:1:4:1:5
PARSE ERROR - fuzz_crash_017.md:1:6:1:7
PARSE ERROR - fuzz_crash_017.md:1:7:1:10
PARSE ERROR - fuzz_crash_017.md:1:10:1:11
PARSE ERROR - fuzz_crash_017.md:2:7:2:8
UNKNOWN OPERATOR - fuzz_crash_017.md:2:7:2:20
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_017.md:1:1:1:3:**
```roc
me = "luc"
```
^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_017.md:1:4:1:5:**
```roc
me = "luc"
```
   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_017.md:1:6:1:7:**
```roc
me = "luc"
```
     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_017.md:1:7:1:10:**
```roc
me = "luc"
```
      ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_017.md:1:10:1:11:**
```roc
me = "luc"
```
         ^


**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_017.md:2:7:2:8:**
```roc
foo = "hello ${namF
```
      ^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!

**fuzz_crash_017.md:2:7:2:20:**
```roc
foo = "hello ${namF
```
      ^^^^^^^^^^^^^

Check the spelling and make sure you're using a valid Roc operator like `+`, `-`, `==`.

# TOKENS
~~~zig
LowerIdent(1:1-1:3),OpAssign(1:4-1:5),StringStart(1:6-1:7),StringPart(1:7-1:10),StringEnd(1:10-1:11),
LowerIdent(2:1-2:4),OpAssign(2:5-2:6),StringStart(2:7-2:8),StringPart(2:8-2:14),OpenStringInterpolation(2:14-2:16),LowerIdent(2:16-2:20),EndOfFile(2:20-2:20),
~~~
# PARSE
~~~clojure
(file @1.1-2.20
	(malformed-header @1.1-1.3 (tag "missing_header"))
	(statements
		(s-malformed @1.4-1.5 (tag "statement_unexpected_token"))
		(s-malformed @1.6-1.7 (tag "statement_unexpected_token"))
		(s-malformed @1.7-1.10 (tag "statement_unexpected_token"))
		(s-malformed @1.10-1.11 (tag "statement_unexpected_token"))
		(s-decl @2.1-2.20
			(p-ident @2.1-2.4 (raw "foo"))
			(e-malformed @2.7-2.20 (reason "string_expected_close_interpolation")))))
~~~
# FORMATTED
~~~roc

foo = 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.4 (ident "foo"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.4 (type "Error")))
	(expressions
		(expr @2.7-2.20 (type "Error"))))
~~~
