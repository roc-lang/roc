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
UNCLOSED STRING - :0:0:0:0
MISSING HEADER - fuzz_crash_010.md:1:1:1:2
PARSE ERROR - fuzz_crash_010.md:1:2:1:3
PARSE ERROR - fuzz_crash_010.md:1:3:1:4
PARSE ERROR - fuzz_crash_010.md:1:4:1:5
PARSE ERROR - fuzz_crash_010.md:2:6:2:7
PARSE ERROR - fuzz_crash_010.md:5:35:5:35
# PROBLEMS
**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.



**UNCLOSED STRING**
This string is missing a closing quote.

```roc
    "on        (string 'onmo %')))
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

**fuzz_crash_010.md:1:1:1:2:**
```roc
H{o,
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_010.md:1:2:1:3:**
```roc
H{o,
```
 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_010.md:1:3:1:4:**
```roc
H{o,
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_010.md:1:4:1:5:**
```roc
H{o,
```
   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_010.md:2:6:2:7:**
```roc
    ]
```
     ^


**PARSE ERROR**
A parsing error occurred: `string_unclosed`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_010.md:5:35:5:35:**
```roc
    "on        (string 'onmo %')))
```
                                  ^


# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:3-1:4),Comma(1:4-1:5),
CloseSquare(2:6-2:7),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),
StringStart(5:5-5:6),StringPart(5:6-5:35),EndOfFile(5:35-5:35),
~~~
# PARSE
~~~clojure
(file @1.1-5.35
	(malformed-header @1.1-1.2 (tag "missing_header"))
	(statements
		(s-malformed @1.2-1.3 (tag "statement_unexpected_token"))
		(s-malformed @1.3-1.4 (tag "statement_unexpected_token"))
		(s-malformed @1.4-1.5 (tag "statement_unexpected_token"))
		(s-malformed @2.6-2.7 (tag "statement_unexpected_token"))
		(s-decl @3.1-5.35
			(p-ident @3.1-3.4 (raw "foo"))
			(e-string @5.5-5.35
				(e-string-part @5.6-5.35 (raw "on        (string 'onmo %')))"))))))
~~~
# FORMATTED
~~~roc


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
