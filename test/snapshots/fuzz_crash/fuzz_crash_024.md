# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
{ pf: platform ".-/main._]where # A

#el
var t= ]

#el
var t= 0
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
PARSE ERROR - fuzz_crash_024.md:1:1:1:2
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_024.md:1:7:1:15
PARSE ERROR - fuzz_crash_024.md:1:16:1:17
PARSE ERROR - fuzz_crash_024.md:1:17:1:36
PARSE ERROR - fuzz_crash_024.md:1:36:1:36
PARSE ERROR - fuzz_crash_024.md:4:1:4:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_024.md:4:8:4:9
PARSE ERROR - fuzz_crash_024.md:7:1:7:4
MISSING MAIN! FUNCTION - fuzz_crash_024.md:1:1:7:9
MALFORMED TYPE - fuzz_crash_024.md:1:7:1:15
UNRECOGNIZED SYNTAX - fuzz_crash_024.md:4:8:4:9
DUPLICATE DEFINITION - fuzz_crash_024.md:7:5:7:6
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

```roc
{ pf: platform ".-/main._]where # A
```
               ^^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_024.md:1:1:1:2:**
```roc
{ pf: platform ".-/main._]where # A
```
^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **platform** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**fuzz_crash_024.md:1:7:1:15:**
```roc
{ pf: platform ".-/main._]where # A
```
      ^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_024.md:1:16:1:17:**
```roc
{ pf: platform ".-/main._]where # A
```
               ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_024.md:1:17:1:36:**
```roc
{ pf: platform ".-/main._]where # A
```
                ^^^^^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_024.md:1:36:1:36:**
```roc
{ pf: platform ".-/main._]where # A
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


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_crash_024.md:1:1:7:9:**
```roc
{ pf: platform ".-/main._]where # A

#el
var t= ]

#el
var t= 0
```


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**fuzz_crash_024.md:1:7:1:15:**
```roc
{ pf: platform ".-/main._]where # A
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
OpenCurly(1:1-1:2),LowerIdent(1:3-1:5),OpColon(1:5-1:6),KwPlatform(1:7-1:15),StringStart(1:16-1:17),StringPart(1:17-1:36),StringEnd(1:36-1:36),
KwVar(4:1-4:4),LowerIdent(4:5-4:6),OpAssign(4:6-4:7),CloseSquare(4:8-4:9),
KwVar(7:1-7:4),LowerIdent(7:5-7:6),OpAssign(7:6-7:7),Int(7:8-7:9),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.9
	(type-module @1.1-1.2)
	(statements
		(s-malformed @1.1-1.2 (tag "statement_unexpected_token"))
		(s-type-anno @1.3-1.15 (name "pf")
			(ty-malformed @1.7-1.15 (tag "ty_anno_unexpected_token")))
		(s-malformed @1.16-1.17 (tag "statement_unexpected_token"))
		(s-malformed @1.17-1.36 (tag "statement_unexpected_token"))
		(s-malformed @1.36-1.36 (tag "statement_unexpected_token"))
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
