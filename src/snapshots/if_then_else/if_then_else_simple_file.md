# META
~~~ini
description=Example if-then-else statement
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo = if 1 A

    else {
	"hello"
    }
~~~
# EXPECTED
PARSE ERROR - if_then_else_simple_file.md:1:1:1:1
UNEXPECTED TOKEN IN EXPRESSION - if_then_else_simple_file.md:5:5:5:11
INVALID STATEMENT - if_then_else_simple_file.md:5:5:5:11
INVALID STATEMENT - if_then_else_simple_file.md:5:10:7:6
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**if_then_else_simple_file.md:1:1:1:1:**
```roc
module [foo]
```



**UNEXPECTED TOKEN IN EXPRESSION**
The token **else {** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**if_then_else_simple_file.md:5:5:5:11:**
```roc
    else {
```
    ^^^^^^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**if_then_else_simple_file.md:5:5:5:11:**
```roc
    else {
```
    ^^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**if_then_else_simple_file.md:5:10:7:6:**
```roc
    else {
	"hello"
    }
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),KwIf(3:7-3:9),Int(3:10-3:11),UpperIdent(3:12-3:13),Newline(1:1-1:1),
Newline(1:1-1:1),
KwElse(5:5-5:9),OpenCurly(5:10-5:11),Newline(1:1-1:1),
StringStart(6:2-6:3),StringPart(6:3-6:8),StringEnd(6:8-6:9),Newline(1:1-1:1),
CloseCurly(7:5-7:6),EndOfFile(7:6-7:6),
~~~
# PARSE
~~~clojure
(file @1.1-7.6
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident (text "foo"))))
	(statements
		(s-decl @1.1-1.1
			(p-ident @3.1-3.4 (raw "foo"))
			(e-malformed @1.1-1.1 (reason "no_else")))
		(e-malformed @5.5-5.11 (reason "expr_unexpected_token"))
		(e-block @5.10-7.6
			(statements
				(e-string @6.2-6.9
					(e-string-part @6.3-6.8 (raw "hello")))))))
~~~
# FORMATTED
~~~roc
module [foo]

foo = 

{
	"hello"
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.4 (ident "foo"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.1 (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
