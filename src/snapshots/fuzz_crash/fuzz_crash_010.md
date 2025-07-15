# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
H{o,
    ]
foo =

    "on        (string 'onmo %')))
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
MISMATCHED BRACE - :0:0:0:0
UNCLOSED STRING - :0:0:0:0
MISSING HEADER - fuzz_crash_010.md:1:1:1:2
PARSE ERROR - fuzz_crash_010.md:1:2:1:3
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_010.md:1:4:1:5
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_010.md:2:6:2:7
PARSE ERROR - fuzz_crash_010.md:5:35:5:35
INVALID STATEMENT - fuzz_crash_010.md:1:3:1:4
INVALID STATEMENT - fuzz_crash_010.md:1:4:1:5
INVALID STATEMENT - fuzz_crash_010.md:2:6:2:7
# PROBLEMS
**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.

**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**UNCLOSED STRING**
This string is missing a closing quote.

**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_010.md:1:1:1:2:**
```roc
H{o,
```
^


**PARSE ERROR**
A parsing error occurred: `unexpected_top_level_open_curly`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_010.md:1:2:1:3:**
```roc
H{o,
```
 ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_010.md:1:4:1:5:**
```roc
H{o,
```
   ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_010.md:2:6:2:7:**
```roc
    ]
```
     ^


**PARSE ERROR**
A parsing error occurred: `string_unclosed`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_010.md:5:35:5:35:**
```roc
    "on        (string 'onmo %')))
```
                                  


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_010.md:1:3:1:4:**
```roc
H{o,
```
  ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_010.md:1:4:1:5:**
```roc
H{o,
```
   ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_010.md:2:6:2:7:**
```roc
    ]
```
     ^


# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:3-1:4),Comma(1:4-1:5),
CloseCurly(2:6-2:7),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),
StringStart(5:5-5:6),StringPart(5:6-5:35),EndOfFile(5:35-5:35),
~~~
# PARSE
~~~clojure
(file @1.1-5.35
	(malformed-header @1.1-1.2 (tag "missing_header"))
	(statements
		(s-malformed @1.2-1.3 (tag "unexpected_top_level_open_curly"))
		(e-ident @1.3-1.4 (raw "o"))
		(e-malformed @1.4-1.5 (reason "expr_unexpected_token"))
		(e-malformed @2.6-2.7 (reason "expr_unexpected_token"))
		(s-decl @3.1-5.35
			(p-ident @3.1-3.4 (raw "foo"))
			(e-string @5.5-5.35
				(e-string-part @5.6-5.35 (raw "on        (string 'onmo %')))"))))))
~~~
# FORMATTED
~~~roc
o


foo = 

	"on        (string 'onmo %')))"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.4 (ident "foo"))
		(e-string @5.5-5.35
			(e-literal @5.6-5.35 (string "on        (string 'onmo %')))")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.4 (type "Str")))
	(expressions
		(expr @5.5-5.35 (type "Str"))))
~~~
