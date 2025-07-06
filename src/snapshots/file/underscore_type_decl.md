# META
~~~ini
description=underscore_in_assignment_pattern
type=file
~~~
# SOURCE
~~~roc
module []

import Module exposing [Pair]

Pair1(x, _) = Pair(0, 1)
Pair2(_, y) = Pair(0, 1)
Pair3(_, _) = Pair(0, 1)
~~~
# EXPECTED
PARSE ERROR - underscore_type_decl.md:5:10:5:12
PARSE ERROR - underscore_type_decl.md:5:1:5:7
PARSE ERROR - underscore_type_decl.md:5:20:5:22
PARSE ERROR - underscore_type_decl.md:5:23:5:25
PARSE ERROR - underscore_type_decl.md:5:15:5:20
UNEXPECTED TOKEN IN EXPRESSION - underscore_type_decl.md:6:7:6:9
UNEXPECTED TOKEN IN EXPRESSION - underscore_type_decl.md:6:13:6:19
PARSE ERROR - underscore_type_decl.md:6:20:6:22
PARSE ERROR - underscore_type_decl.md:6:23:6:25
PARSE ERROR - underscore_type_decl.md:6:15:6:20
UNEXPECTED TOKEN IN EXPRESSION - underscore_type_decl.md:7:7:7:9
UNEXPECTED TOKEN IN EXPRESSION - underscore_type_decl.md:7:10:7:12
UNEXPECTED TOKEN IN EXPRESSION - underscore_type_decl.md:7:13:7:19
PARSE ERROR - underscore_type_decl.md:7:20:7:22
PARSE ERROR - underscore_type_decl.md:7:23:7:25
PARSE ERROR - underscore_type_decl.md:7:15:7:20
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**underscore_type_decl.md:5:10:5:12:**
```roc
Pair1(x, _) = Pair(0, 1)
```
         ^^


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
**underscore_type_decl.md:5:1:5:7:**
```roc
Pair1(x, _) = Pair(0, 1)
```
^^^^^^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**underscore_type_decl.md:5:20:5:22:**
```roc
Pair1(x, _) = Pair(0, 1)
```
                   ^^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**underscore_type_decl.md:5:23:5:25:**
```roc
Pair1(x, _) = Pair(0, 1)
```
                      ^^


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
**underscore_type_decl.md:5:15:5:20:**
```roc
Pair1(x, _) = Pair(0, 1)
```
              ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **_,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**underscore_type_decl.md:6:7:6:9:**
```roc
Pair2(_, y) = Pair(0, 1)
```
      ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **= Pair** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**underscore_type_decl.md:6:13:6:19:**
```roc
Pair2(_, y) = Pair(0, 1)
```
            ^^^^^^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**underscore_type_decl.md:6:20:6:22:**
```roc
Pair2(_, y) = Pair(0, 1)
```
                   ^^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**underscore_type_decl.md:6:23:6:25:**
```roc
Pair2(_, y) = Pair(0, 1)
```
                      ^^


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
**underscore_type_decl.md:6:15:6:20:**
```roc
Pair2(_, y) = Pair(0, 1)
```
              ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **_,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**underscore_type_decl.md:7:7:7:9:**
```roc
Pair3(_, _) = Pair(0, 1)
```
      ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **_)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**underscore_type_decl.md:7:10:7:12:**
```roc
Pair3(_, _) = Pair(0, 1)
```
         ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **= Pair** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**underscore_type_decl.md:7:13:7:19:**
```roc
Pair3(_, _) = Pair(0, 1)
```
            ^^^^^^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**underscore_type_decl.md:7:20:7:22:**
```roc
Pair3(_, _) = Pair(0, 1)
```
                   ^^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**underscore_type_decl.md:7:23:7:25:**
```roc
Pair3(_, _) = Pair(0, 1)
```
                      ^^


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
**underscore_type_decl.md:7:15:7:20:**
```roc
Pair3(_, _) = Pair(0, 1)
```
              ^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),UpperIdent(3:8-3:14),KwExposing(3:15-3:23),OpenSquare(3:24-3:25),UpperIdent(3:25-3:29),CloseSquare(3:29-3:30),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(5:1-5:6),NoSpaceOpenRound(5:6-5:7),LowerIdent(5:7-5:8),Comma(5:8-5:9),Underscore(5:10-5:11),CloseRound(5:11-5:12),OpAssign(5:13-5:14),UpperIdent(5:15-5:19),NoSpaceOpenRound(5:19-5:20),Int(5:20-5:21),Comma(5:21-5:22),Int(5:23-5:24),CloseRound(5:24-5:25),Newline(1:1-1:1),
UpperIdent(6:1-6:6),NoSpaceOpenRound(6:6-6:7),Underscore(6:7-6:8),Comma(6:8-6:9),LowerIdent(6:10-6:11),CloseRound(6:11-6:12),OpAssign(6:13-6:14),UpperIdent(6:15-6:19),NoSpaceOpenRound(6:19-6:20),Int(6:20-6:21),Comma(6:21-6:22),Int(6:23-6:24),CloseRound(6:24-6:25),Newline(1:1-1:1),
UpperIdent(7:1-7:6),NoSpaceOpenRound(7:6-7:7),Underscore(7:7-7:8),Comma(7:8-7:9),Underscore(7:10-7:11),CloseRound(7:11-7:12),OpAssign(7:13-7:14),UpperIdent(7:15-7:19),NoSpaceOpenRound(7:19-7:20),Int(7:20-7:21),Comma(7:21-7:22),Int(7:23-7:24),CloseRound(7:24-7:25),EndOfFile(7:25-7:25),
~~~
# PARSE
~~~clojure
(file @1.1-7.25
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-import @3.1-3.30 (raw "Module")
			(exposing
				(exposed-upper-ident (text "Pair"))))
		(s-malformed @5.1-5.19 (tag "expected_colon_after_type_annotation"))
		(s-malformed @5.15-6.7 (tag "expected_colon_after_type_annotation"))
		(e-tuple @6.6-6.12
			(e-malformed @6.7-6.9 (reason "expr_unexpected_token"))
			(e-ident @6.10-6.11 (raw "y")))
		(e-malformed @6.13-6.19 (reason "expr_unexpected_token"))
		(s-malformed @6.15-7.7 (tag "expected_colon_after_type_annotation"))
		(e-tuple @7.6-7.12
			(e-malformed @7.7-7.9 (reason "expr_unexpected_token"))
			(e-malformed @7.10-7.12 (reason "expr_unexpected_token")))
		(e-malformed @7.13-7.19 (reason "expr_unexpected_token"))
		(s-malformed @7.15-7.25 (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
module []

import Module exposing [Pair]

(, y)(, )
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import @3.1-3.30 (module "Module")
		(exposes
			(exposed (name "Pair") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
