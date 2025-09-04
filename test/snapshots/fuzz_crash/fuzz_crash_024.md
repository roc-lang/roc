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
PARSE ERROR - fuzz_crash_024.md:1:9:1:15
PARSE ERROR - fuzz_crash_024.md:1:18:1:19
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_024.md:1:24:1:32
PARSE ERROR - fuzz_crash_024.md:1:33:1:34
PARSE ERROR - fuzz_crash_024.md:1:34:1:53
PARSE ERROR - fuzz_crash_024.md:1:53:1:53
PARSE ERROR - fuzz_crash_024.md:4:1:4:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_024.md:4:8:4:9
PARSE ERROR - fuzz_crash_024.md:7:1:7:4
MALFORMED TYPE - fuzz_crash_024.md:1:24:1:32
UNRECOGNIZED SYNTAX - fuzz_crash_024.md:4:8:4:9
DUPLICATE DEFINITION - fuzz_crash_024.md:7:5:7:6
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

```roc
module [module ] { pf: platform ".-/main._]where # A
```
                                ^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `exposed_item_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_024.md:1:9:1:15:**
```roc
module [module ] { pf: platform ".-/main._]where # A
```
        ^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_024.md:1:18:1:19:**
```roc
module [module ] { pf: platform ".-/main._]where # A
```
                 ^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **platform** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**fuzz_crash_024.md:1:24:1:32:**
```roc
module [module ] { pf: platform ".-/main._]where # A
```
                       ^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_024.md:1:33:1:34:**
```roc
module [module ] { pf: platform ".-/main._]where # A
```
                                ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_024.md:1:34:1:53:**
```roc
module [module ] { pf: platform ".-/main._]where # A
```
                                 ^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_024.md:1:53:1:53:**
```roc
module [module ] { pf: platform ".-/main._]where # A
```
                                                    ^


**PARSE ERROR**
A parsing error occurred: `var_only_allowed_in_a_body`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_024.md:4:1:4:4:**
```roc
var t= ]
```
^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_024.md:4:8:4:9:**
```roc
var t= ]
```
       ^


**PARSE ERROR**
A parsing error occurred: `var_only_allowed_in_a_body`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_024.md:7:1:7:4:**
```roc
var t= 0
```
^^^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**fuzz_crash_024.md:1:24:1:32:**
```roc
module [module ] { pf: platform ".-/main._]where # A
```
                       ^^^^^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**fuzz_crash_024.md:4:8:4:9:**
```roc
var t= ]
```
       ^

This might be a syntax error, an unsupported language feature, or a typo.

**DUPLICATE DEFINITION**
The name `t` is being redeclared in this scope.

The redeclaration is here:
**fuzz_crash_024.md:7:5:7:6:**
```roc
var t= 0
```
    ^

But `t` was already defined here:
**fuzz_crash_024.md:4:5:4:6:**
```roc
var t= ]
```
    ^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),KwModule(1:9-1:15),CloseSquare(1:16-1:17),OpenCurly(1:18-1:19),LowerIdent(1:20-1:22),OpColon(1:22-1:23),KwPlatform(1:24-1:32),StringStart(1:33-1:34),StringPart(1:34-1:53),StringEnd(1:53-1:53),
KwVar(4:1-4:4),LowerIdent(4:5-4:6),OpAssign(4:6-4:7),CloseSquare(4:8-4:9),
KwVar(7:1-7:4),LowerIdent(7:5-7:6),OpAssign(7:6-7:7),Int(7:8-7:9),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.9
	(module @1.1-1.17
		(exposes @1.8-1.17
			(exposed-malformed @1.9-1.15 (reason "exposed_item_unexpected_token") @1.9-1.15)))
	(statements
		(s-malformed @1.18-1.19 (tag "statement_unexpected_token"))
		(s-type-anno @1.20-1.32 (name "pf")
			(ty-malformed @1.24-1.32 (tag "ty_anno_unexpected_token")))
		(s-malformed @1.33-1.34 (tag "statement_unexpected_token"))
		(s-malformed @1.34-1.53 (tag "statement_unexpected_token"))
		(s-malformed @1.53-1.53 (tag "statement_unexpected_token"))
		(s-malformed @4.1-4.4 (tag "var_only_allowed_in_a_body"))
		(s-decl @4.5-4.9
			(p-ident @4.5-4.6 (raw "t"))
			(e-malformed @4.8-4.9 (reason "expr_unexpected_token")))
		(s-malformed @7.1-7.4 (tag "var_only_allowed_in_a_body"))
		(s-decl @7.5-7.9
			(p-ident @7.5-7.6 (raw "t"))
			(e-int @7.8-7.9 (raw "0")))))
~~~
# FORMATTED
~~~roc
module []
pf : 


# el
t = 

# el
t = 0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.5-4.6 (ident "t"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(d-let
		(p-assign @7.5-7.6 (ident "t"))
		(e-int @7.8-7.9 (value "0"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.5-4.6 (type "Error"))
		(patt @7.5-7.6 (type "Num(_size)")))
	(expressions
		(expr @4.8-4.9 (type "Error"))
		(expr @7.8-7.9 (type "Num(_size)"))))
~~~
