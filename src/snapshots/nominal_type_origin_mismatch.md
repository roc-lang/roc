# META
~~~ini
description=Type mismatch showing nominal type origin from different module
type=file
~~~
# SOURCE
~~~roc
module []

import Data exposing [Person]

expectsPerson : Person -> Str
expectsPerson = \p -> "Got a person"

main =
    # This will cause a type mismatch
    expectsPerson("not a person")
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - nominal_type_origin_mismatch.md:6:17:6:18
PARSE ERROR - nominal_type_origin_mismatch.md:6:23:6:24
UNEXPECTED TOKEN IN EXPRESSION - nominal_type_origin_mismatch.md:6:24:6:36
UNEXPECTED TOKEN IN EXPRESSION - nominal_type_origin_mismatch.md:6:36:6:37
UNDECLARED TYPE - nominal_type_origin_mismatch.md:5:17:5:23
INVALID STATEMENT - nominal_type_origin_mismatch.md:6:18:6:24
INVALID STATEMENT - nominal_type_origin_mismatch.md:6:24:6:36
INVALID STATEMENT - nominal_type_origin_mismatch.md:6:36:6:37
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **\** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_type_origin_mismatch.md:6:17:6:18:**
```roc
expectsPerson = \p -> "Got a person"
```
                ^


**PARSE ERROR**
A parsing error occurred: `expr_arrow_expects_ident`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**nominal_type_origin_mismatch.md:6:23:6:24:**
```roc
expectsPerson = \p -> "Got a person"
```
                      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **Got a person** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_type_origin_mismatch.md:6:24:6:36:**
```roc
expectsPerson = \p -> "Got a person"
```
                       ^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_type_origin_mismatch.md:6:36:6:37:**
```roc
expectsPerson = \p -> "Got a person"
```
                                   ^


**UNDECLARED TYPE**
The type ``Person`` is not declared in this scope.

This type is referenced here:
**nominal_type_origin_mismatch.md:5:17:5:23:**
```roc
expectsPerson : Person -> Str
```
                ^^^^^^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**nominal_type_origin_mismatch.md:6:18:6:24:**
```roc
expectsPerson = \p -> "Got a person"
```
                 ^^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**nominal_type_origin_mismatch.md:6:24:6:36:**
```roc
expectsPerson = \p -> "Got a person"
```
                       ^^^^^^^^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**nominal_type_origin_mismatch.md:6:36:6:37:**
```roc
expectsPerson = \p -> "Got a person"
```
                                   ^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
KwImport(3:1-3:7),UpperIdent(3:8-3:12),KwExposing(3:13-3:21),OpenSquare(3:22-3:23),UpperIdent(3:23-3:29),CloseSquare(3:29-3:30),
LowerIdent(5:1-5:14),OpColon(5:15-5:16),UpperIdent(5:17-5:23),OpArrow(5:24-5:26),UpperIdent(5:27-5:30),
LowerIdent(6:1-6:14),OpAssign(6:15-6:16),OpBackslash(6:17-6:18),LowerIdent(6:18-6:19),OpArrow(6:20-6:22),StringStart(6:23-6:24),StringPart(6:24-6:36),StringEnd(6:36-6:37),
LowerIdent(8:1-8:5),OpAssign(8:6-8:7),
LowerIdent(10:5-10:18),NoSpaceOpenRound(10:18-10:19),StringStart(10:19-10:20),StringPart(10:20-10:32),StringEnd(10:32-10:33),CloseRound(10:33-10:34),EndOfFile(10:34-10:34),
~~~
# PARSE
~~~clojure
(file @1.1-10.34
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-import @3.1-3.30 (raw "Data")
			(exposing
				(exposed-upper-ident @3.23-3.29 (text "Person"))))
		(s-type-anno @5.1-5.30 (name "expectsPerson")
			(ty-fn @5.17-5.30
				(ty @5.17-5.23 (name "Person"))
				(ty @5.27-5.30 (name "Str"))))
		(s-decl @6.1-6.18
			(p-ident @6.1-6.14 (raw "expectsPerson"))
			(e-malformed @6.17-6.18 (reason "expr_unexpected_token")))
		(e-malformed @6.23-6.24 (reason "expr_arrow_expects_ident"))
		(e-malformed @6.24-6.36 (reason "expr_unexpected_token"))
		(e-malformed @6.36-6.37 (reason "expr_unexpected_token"))
		(s-decl @8.1-10.34
			(p-ident @8.1-8.5 (raw "main"))
			(e-apply @10.5-10.34
				(e-ident @10.5-10.18 (raw "expectsPerson"))
				(e-string @10.19-10.33
					(e-string-part @10.20-10.32 (raw "not a person")))))))
~~~
# FORMATTED
~~~roc
module []

import Data exposing [Person]

expectsPerson : Person -> Str
expectsPerson = 


main = 
# This will cause a type mismatch
	expectsPerson("not a person")
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.14 (ident "expectsPerson"))
		(e-runtime-error (tag "expr_not_canonicalized"))
		(annotation @6.1-6.14
			(declared-type
				(ty-fn @5.17-5.30 (effectful false)
					(ty @5.17-5.23 (name "Person"))
					(ty @5.27-5.30 (name "Str"))))))
	(d-let
		(p-assign @8.1-8.5 (ident "main"))
		(e-call @10.5-10.34
			(e-lookup-local @10.5-10.18
				(p-assign @6.1-6.14 (ident "expectsPerson")))
			(e-string @10.19-10.33
				(e-literal @10.20-10.32 (string "not a person")))))
	(s-import @3.1-3.30 (module "Data")
		(exposes
			(exposed (name "Person") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.14 (type "Error"))
		(patt @8.1-8.5 (type "a")))
	(expressions
		(expr @6.17-6.18 (type "Error"))
		(expr @10.5-10.34 (type "a"))))
~~~
