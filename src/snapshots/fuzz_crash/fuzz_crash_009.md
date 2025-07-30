# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
 f{o,
     ]

foo =

    "onmo %
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
MISSING HEADER - fuzz_crash_009.md:1:2:1:3
PARSE ERROR - fuzz_crash_009.md:1:3:1:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_009.md:1:5:1:6
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_009.md:2:6:2:7
PARSE ERROR - fuzz_crash_009.md:6:12:6:12
INVALID STATEMENT - fuzz_crash_009.md:1:4:1:5
INVALID STATEMENT - fuzz_crash_009.md:1:5:1:6
INVALID STATEMENT - fuzz_crash_009.md:2:6:2:7
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_009.md:1:2:1:3:**
```roc
 f{o,
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_009.md:1:3:1:4:**
```roc
 f{o,
```
  ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_009.md:1:5:1:6:**
```roc
 f{o,
```
    ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_009.md:2:6:2:7:**
```roc
     ]
```
     ^


**PARSE ERROR**
A parsing error occurred: `string_unclosed`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_009.md:6:12:6:12:**
```roc
    "onmo %
```
           


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_009.md:1:4:1:5:**
```roc
 f{o,
```
   ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_009.md:1:5:1:6:**
```roc
 f{o,
```
    ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_009.md:2:6:2:7:**
```roc
     ]
```
     ^


# TOKENS
~~~zig
LowerIdent(1:2-1:3),OpenCurly(1:3-1:4),LowerIdent(1:4-1:5),Comma(1:5-1:6),
CloseSquare(2:6-2:7),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),
StringStart(6:5-6:6),StringPart(6:6-6:12),EndOfFile(6:12-6:12),
~~~
# PARSE
~~~clojure
(file @1.2-6.12
	(malformed-header @1.2-1.3 (tag "missing_header"))
	(statements
		(s-malformed @1.3-1.4 (tag "statement_unexpected_token"))
		(e-ident @1.4-1.5 (raw "o"))
		(e-malformed @1.5-1.6 (reason "expr_unexpected_token"))
		(e-malformed @2.6-2.7 (reason "expr_unexpected_token"))
		(s-decl @4.1-6.12
			(p-ident @4.1-4.4 (raw "foo"))
			(e-string @6.5-6.12
				(e-string-part @6.6-6.12 (raw "onmo %"))))))
~~~
# FORMATTED
~~~roc
o



foo = 

	"onmo %"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.4 (ident "foo"))
		(e-string @6.5-6.12
			(e-literal @6.6-6.12 (string "onmo %")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "Str")))
	(expressions
		(expr @6.5-6.12 (type "Str"))))
~~~
