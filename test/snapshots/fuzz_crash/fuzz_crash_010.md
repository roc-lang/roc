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
PARSE ERROR - fuzz_crash_010.md:1:2:1:3
PARSE ERROR - fuzz_crash_010.md:1:3:1:4
PARSE ERROR - fuzz_crash_010.md:1:4:1:5
PARSE ERROR - fuzz_crash_010.md:2:6:2:7
TYPE MODULE MISSING MATCHING TYPE - fuzz_crash_010.md:1:1:5:35
# PROBLEMS
**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.



**UNCLOSED STRING**
This string is missing a closing quote.

```roc
    "on        (string 'onmo %')))
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

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


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This module is named `fuzz_crash_010`, but no top-level type declaration named `fuzz_crash_010` was found.

Add either:
`fuzz_crash_010 := ...` (nominal type)
or:
`fuzz_crash_010 : ...` (type alias)
**fuzz_crash_010.md:1:1:5:35:**
```roc
H{o,
    ]
foo =

    "on        (string 'onmo %')))
```


# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:3-1:4),Comma(1:4-1:5),
CloseSquare(2:6-2:7),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),
StringStart(5:5-5:6),StringPart(5:6-5:35),StringEnd(5:35-5:35),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.35
	(type-module @1.1-1.2)
	(statements
		(s-malformed @1.2-1.3 (tag "expected_colon_after_type_annotation"))
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
