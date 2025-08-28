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
PARSE ERROR - fuzz_crash_009.md:1:4:1:5
PARSE ERROR - fuzz_crash_009.md:1:5:1:6
PARSE ERROR - fuzz_crash_009.md:2:6:2:7
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

```roc
    "onmo %
```
    ^^^^^^^


**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

**fuzz_crash_009.md:1:2:1:3:**
```roc
 f{o,
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_009.md:1:3:1:4:**
```roc
 f{o,
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_009.md:1:4:1:5:**
```roc
 f{o,
```
   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_009.md:1:5:1:6:**
```roc
 f{o,
```
    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

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
StringStart(6:5-6:6),StringPart(6:6-6:12),StringEnd(6:12-6:12),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.2-6.12
	(malformed-header @1.2-1.3 (tag "missing_header"))
	(statements
		(s-malformed @1.3-1.4 (tag "statement_unexpected_token"))
		(s-malformed @1.4-1.5 (tag "statement_unexpected_token"))
		(s-malformed @1.5-1.6 (tag "statement_unexpected_token"))
		(s-malformed @2.6-2.7 (tag "statement_unexpected_token"))
		(s-decl @4.1-6.12
			(p-ident @4.1-4.4 (raw "foo"))
			(e-string @6.5-6.12
				(e-string-part @6.6-6.12 (raw "onmo %"))))))
~~~
# FORMATTED
~~~roc
MALFORMED INPUT
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
