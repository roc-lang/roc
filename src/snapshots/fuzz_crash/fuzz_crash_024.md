# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module [module ] { pf: platform ".-/main._]where # A

#el
var t= ]

#el
var t= 0
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
MISMATCHED BRACE - :0:0:0:0
PARSE ERROR - fuzz_crash_024.md:1:9:1:15
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_024.md:1:24:1:32
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_024.md:4:8:4:9
PARSE ERROR - fuzz_crash_024.md:7:9:7:9
INVALID STATEMENT - fuzz_crash_024.md:1:18:7:9
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**PARSE ERROR**
A parsing error occurred: `exposed_item_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_024.md:1:9:1:15:**
```roc
module [module ] { pf: platform ".-/main._]where # A
```
        ^^^^^^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **platform** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**fuzz_crash_024.md:1:24:1:32:**
```roc
module [module ] { pf: platform ".-/main._]where # A
```
                       ^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_024.md:4:8:4:9:**
```roc
var t= ]
```
       ^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_024.md:7:9:7:9:**
```roc
var t= 0
```
        


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_024.md:1:18:7:9:**
```roc
module [module ] { pf: platform ".-/main._]where # A

#el
var t= ]

#el
var t= 0
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),KwModule(1:9-1:15),CloseSquare(1:16-1:17),OpenCurly(1:18-1:19),LowerIdent(1:20-1:22),OpColon(1:22-1:23),KwPlatform(1:24-1:32),StringStart(1:33-1:34),StringPart(1:34-1:53),StringEnd(1:53-1:53),
KwVar(4:1-4:4),LowerIdent(4:5-4:6),OpAssign(4:6-4:7),CloseCurly(4:8-4:9),
KwVar(7:1-7:4),LowerIdent(7:5-7:6),OpAssign(7:6-7:7),Int(7:8-7:9),EndOfFile(7:9-7:9),
~~~
# PARSE
~~~clojure
(file @1.1-7.9
	(module @1.1-1.17
		(exposes @1.8-1.17
			(exposed-malformed @1.9-1.15 (reason "exposed_item_unexpected_token") @1.9-1.15)))
	(statements
		(e-block @1.18-7.9
			(statements
				(s-type-anno @1.20-1.32 (name "pf")
					(ty-malformed @1.24-1.32 (tag "ty_anno_unexpected_token")))
				(e-string @1.33-1.53
					(e-string-part @1.34-1.53 (raw ".-/main._]where # A")))
				(s-var @4.1-4.9 (name "t")
					(e-malformed @4.8-4.9 (reason "expr_unexpected_token")))
				(s-var @7.1-7.9 (name "t")
					(e-int @7.8-7.9 (raw "0")))))))
~~~
# FORMATTED
~~~roc
module []
{
	pf : 
	".-/main._]where # A"

	# el
	var t = 

	# el
	var t = 0
}
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
