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
PARSE ERROR - fuzz_crash_009.md:2:6:2:7
PARSE ERROR - fuzz_crash_009.md:4:1:4:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_009.md:4:5:4:6
PARSE ERROR - fuzz_crash_009.md:6:12:6:12
INVALID STATEMENT - fuzz_crash_009.md:1:3:4:4
INVALID STATEMENT - fuzz_crash_009.md:4:5:4:6
INVALID STATEMENT - fuzz_crash_009.md:6:5:6:12
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
A parsing error occurred: `expected_expr_record_field_name`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_009.md:2:6:2:7:**
```roc
     ]
```
     ^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_009.md:4:1:4:4:**
```roc
foo =
```
^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_009.md:4:5:4:6:**
```roc
foo =
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

**fuzz_crash_009.md:1:3:4:4:**
```roc
 f{o,
     ]

foo =
```


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_009.md:4:5:4:6:**
```roc
foo =
```
    ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_009.md:6:5:6:12:**
```roc
    "onmo %
```
    ^^^^^^^


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
		(e-malformed @4.1-4.4 (reason "expected_expr_close_curly_or_comma"))
		(e-malformed @4.5-4.6 (reason "expr_unexpected_token"))
		(e-string @6.5-6.12
			(e-string-part @6.6-6.12 (raw "onmo %")))))
~~~
# FORMATTED
~~~roc


"onmo %"
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
