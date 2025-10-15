# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
#el
var t= ]

#el
var t= 0
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_024.md:2:1:2:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_024.md:2:8:2:9
PARSE ERROR - fuzz_crash_024.md:5:1:5:4
UNRECOGNIZED SYNTAX - fuzz_crash_024.md:2:8:2:9
DUPLICATE DEFINITION - fuzz_crash_024.md:5:5:5:6
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `var_only_allowed_in_a_body`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_024.md:2:1:2:4:**
```roc
var t= ]
```
^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_024.md:2:8:2:9:**
```roc
var t= ]
```
       ^


**PARSE ERROR**
A parsing error occurred: `var_only_allowed_in_a_body`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_024.md:5:1:5:4:**
```roc
var t= 0
```
^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**fuzz_crash_024.md:2:8:2:9:**
```roc
var t= ]
```
       ^

This might be a syntax error, an unsupported language feature, or a typo.

**DUPLICATE DEFINITION**
The name `t` is being redeclared in this scope.

The redeclaration is here:
**fuzz_crash_024.md:5:5:5:6:**
```roc
var t= 0
```
    ^

But `t` was already defined here:
**fuzz_crash_024.md:2:5:2:6:**
```roc
var t= ]
```
    ^


# TOKENS
~~~zig
KwVar(2:1-2:4),LowerIdent(2:5-2:6),OpAssign(2:6-2:7),CloseSquare(2:8-2:9),
KwVar(5:1-5:4),LowerIdent(5:5-5:6),OpAssign(5:6-5:7),Int(5:8-5:9),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @2.1-5.9
	(type-module @2.1-2.4)
	(statements
		(s-malformed @2.1-2.4 (tag "var_only_allowed_in_a_body"))
		(s-decl @2.5-2.9
			(p-ident @2.5-2.6 (raw "t"))
			(e-malformed @2.8-2.9 (reason "expr_unexpected_token")))
		(s-malformed @5.1-5.4 (tag "var_only_allowed_in_a_body"))
		(s-decl @5.5-5.9
			(p-ident @5.5-5.6 (raw "t"))
			(e-int @5.8-5.9 (raw "0")))))
~~~
# FORMATTED
~~~roc
# el
t = 

# el
t = 0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.5-2.6 (ident "t"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(d-let
		(p-assign @5.5-5.6 (ident "t"))
		(e-num @5.8-5.9 (value "0"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.5-2.6 (type "Error"))
		(patt @5.5-5.6 (type "Num(_size)")))
	(expressions
		(expr @2.8-2.9 (type "Error"))
		(expr @5.8-5.9 (type "Num(_size)"))))
~~~
