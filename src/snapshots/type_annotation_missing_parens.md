# META
~~~ini
description=Type annotation missing parentheses for type application
type=file
~~~
# SOURCE
~~~roc
module [nums]

nums : List U8
~~~
~~~
# EXPECTED
PARSE ERROR - type_annotation_missing_parens.md:3:13:3:13
UNEXPECTED TOKEN IN EXPRESSION - type_annotation_missing_parens.md:4:2:4:4
UNEXPECTED TOKEN IN EXPRESSION - type_annotation_missing_parens.md:4:3:4:4
# PROBLEMS
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

Here is the problematic code:
**type_annotation_missing_parens.md:3:13:3:13:**
```roc
nums : List U8
```
            


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_annotation_missing_parens.md:4:2:4:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_annotation_missing_parens.md:4:3:4:4:**
```roc
~~~
```
  ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:13),CloseSquare(1:13-1:14),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:5),OpColon(3:6-3:7),UpperIdent(3:8-3:12),UpperIdent(3:13-3:15),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(file @1.1-4.4
	(module @1.1-1.14
		(exposes @1.8-1.14
			(exposed-lower-ident (text "nums"))))
	(statements
		(s-type-anno @3.1-3.15 (name "nums")
			(ty @3.8-3.12 (name "List")))
		(s-malformed @3.13-4.3 (tag "expected_colon_after_type_annotation"))
		(e-malformed @4.2-4.4 (reason "expr_unexpected_token"))
		(e-malformed @4.3-4.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [nums]

nums : List
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
