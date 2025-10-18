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
KwVar,LowerIdent,OpAssign,CloseSquare,
KwVar,LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "var_only_allowed_in_a_body"))
		(s-decl
			(p-ident (raw "t"))
			(e-malformed (reason "expr_unexpected_token")))
		(s-malformed (tag "var_only_allowed_in_a_body"))
		(s-decl
			(p-ident (raw "t"))
			(e-int (raw "0")))))
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
		(p-assign (ident "t"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(d-let
		(p-assign (ident "t"))
		(e-num (value "0"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Num(_size)")))
	(expressions
		(expr (type "Error"))
		(expr (type "Num(_size)"))))
~~~
