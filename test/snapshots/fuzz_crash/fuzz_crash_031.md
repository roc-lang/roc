# META
~~~ini
description=fuzz crash, unterminated single quote
type=file
~~~
# SOURCE
~~~roc
mule []

#el
vavar t= '
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_031.md:1:1:1:5
PARSE ERROR - fuzz_crash_031.md:1:6:1:7
PARSE ERROR - fuzz_crash_031.md:1:7:1:8
PARSE ERROR - fuzz_crash_031.md:4:1:4:6
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_031.md:4:10:4:11
MISSING MAIN! FUNCTION - fuzz_crash_031.md:1:1:4:11
UNRECOGNIZED SYNTAX - fuzz_crash_031.md:4:10:4:11
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_031.md:1:1:1:5:**
```roc
mule []
```
^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_031.md:1:6:1:7:**
```roc
mule []
```
     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_031.md:1:7:1:8:**
```roc
mule []
```
      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_031.md:4:1:4:6:**
```roc
vavar t= '
```
^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_031.md:4:10:4:11:**
```roc
vavar t= '
```
         ^


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_crash_031.md:1:1:4:11:**
```roc
mule []

#el
vavar t= '
```


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**fuzz_crash_031.md:4:10:4:11:**
```roc
vavar t= '
```
         ^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpenSquare(1:6-1:7),CloseSquare(1:7-1:8),
LowerIdent(4:1-4:6),LowerIdent(4:7-4:8),OpAssign(4:8-4:9),MalformedSingleQuoteUnclosed(4:10-4:11),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.11
	(type-module @1.1-1.5)
	(statements
		(s-malformed @1.1-1.5 (tag "statement_unexpected_token"))
		(s-malformed @1.6-1.7 (tag "statement_unexpected_token"))
		(s-malformed @1.7-1.8 (tag "statement_unexpected_token"))
		(s-malformed @4.1-4.6 (tag "statement_unexpected_token"))
		(s-decl @4.7-4.11
			(p-ident @4.7-4.8 (raw "t"))
			(e-malformed @4.10-4.11 (reason "expr_unexpected_token")))))
~~~
# FORMATTED
~~~roc


# el
t = 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.7-4.8 (ident "t"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.7-4.8 (type "Error")))
	(expressions
		(expr @4.10-4.11 (type "Error"))))
~~~
