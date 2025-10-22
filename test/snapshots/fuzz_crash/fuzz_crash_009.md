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
PARSE ERROR - fuzz_crash_009.md:1:2:1:3
PARSE ERROR - fuzz_crash_009.md:1:3:1:4
PARSE ERROR - fuzz_crash_009.md:1:4:1:5
PARSE ERROR - fuzz_crash_009.md:1:5:1:6
PARSE ERROR - fuzz_crash_009.md:2:6:2:7
MISSING MAIN! FUNCTION - fuzz_crash_009.md:1:2:6:12
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

```roc
    "onmo %
```
    ^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

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


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_crash_009.md:1:2:6:12:**
```roc
 f{o,
     ]

foo =

    "onmo %
```


# TOKENS
~~~zig
LowerIdent,OpenCurly,LowerIdent,Comma,
CloseSquare,
LowerIdent,OpAssign,
StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "foo"))
			(e-string
				(e-string-part (raw "onmo %"))))))
~~~
# FORMATTED
~~~roc



foo = 

	"onmo %"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-string
			(e-literal (string "onmo %")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
