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
PARSE ERROR - underscore_type_decl.md:5:13:5:14
PARSE ERROR - underscore_type_decl.md:5:20:5:21
PARSE ERROR - underscore_type_decl.md:5:23:5:24
PARSE ERROR - underscore_type_decl.md:6:1:6:6
PARSE ERROR - underscore_type_decl.md:6:6:6:7
PARSE ERROR - underscore_type_decl.md:6:7:6:8
PARSE ERROR - underscore_type_decl.md:6:8:6:9
PARSE ERROR - underscore_type_decl.md:6:10:6:11
PARSE ERROR - underscore_type_decl.md:6:11:6:12
PARSE ERROR - underscore_type_decl.md:6:13:6:14
PARSE ERROR - underscore_type_decl.md:6:20:6:21
PARSE ERROR - underscore_type_decl.md:6:23:6:24
PARSE ERROR - underscore_type_decl.md:7:1:7:6
PARSE ERROR - underscore_type_decl.md:7:6:7:7
PARSE ERROR - underscore_type_decl.md:7:7:7:8
PARSE ERROR - underscore_type_decl.md:7:8:7:9
PARSE ERROR - underscore_type_decl.md:7:10:7:11
PARSE ERROR - underscore_type_decl.md:7:11:7:12
PARSE ERROR - underscore_type_decl.md:7:13:7:14
PARSE ERROR - underscore_type_decl.md:7:20:7:21
PARSE ERROR - underscore_type_decl.md:7:23:7:24
PARSE ERROR - underscore_type_decl.md:8:1:8:1
MODULE NOT FOUND - underscore_type_decl.md:3:1:3:30
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

**underscore_type_decl.md:5:13:5:14:**
```roc
Pair1(x, _) = Pair(0, 1)
```
            ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:20:5:21:**
```roc
Pair1(x, _) = Pair(0, 1)
```
                   ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:23:5:24:**
```roc
Pair1(x, _) = Pair(0, 1)
```
                      ^


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

**underscore_type_decl.md:6:1:6:6:**
```roc
Pair2(_, y) = Pair(0, 1)
```
^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:6:6:6:7:**
```roc
Pair2(_, y) = Pair(0, 1)
```
     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:6:7:6:8:**
```roc
Pair2(_, y) = Pair(0, 1)
```
      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:6:8:6:9:**
```roc
Pair2(_, y) = Pair(0, 1)
```
       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:6:10:6:11:**
```roc
Pair2(_, y) = Pair(0, 1)
```
         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:6:11:6:12:**
```roc
Pair2(_, y) = Pair(0, 1)
```
          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:6:13:6:14:**
```roc
Pair2(_, y) = Pair(0, 1)
```
            ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:6:20:6:21:**
```roc
Pair2(_, y) = Pair(0, 1)
```
                   ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:6:23:6:24:**
```roc
Pair2(_, y) = Pair(0, 1)
```
                      ^


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

**underscore_type_decl.md:7:1:7:6:**
```roc
Pair3(_, _) = Pair(0, 1)
```
^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:7:6:7:7:**
```roc
Pair3(_, _) = Pair(0, 1)
```
     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:7:7:7:8:**
```roc
Pair3(_, _) = Pair(0, 1)
```
      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:7:8:7:9:**
```roc
Pair3(_, _) = Pair(0, 1)
```
       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:7:10:7:11:**
```roc
Pair3(_, _) = Pair(0, 1)
```
         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:7:11:7:12:**
```roc
Pair3(_, _) = Pair(0, 1)
```
          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:7:13:7:14:**
```roc
Pair3(_, _) = Pair(0, 1)
```
            ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:7:20:7:21:**
```roc
Pair3(_, _) = Pair(0, 1)
```
                   ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:7:23:7:24:**
```roc
Pair3(_, _) = Pair(0, 1)
```
                      ^


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

**underscore_type_decl.md:8:1:8:1:**
```roc

```
^


**MODULE NOT FOUND**
The module `Module` was not found in this Roc project.

You're attempting to use this module here:
**underscore_type_decl.md:3:1:3:30:**
```roc
import Module exposing [Pair]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
KwImport(3:1-3:7),UpperIdent(3:8-3:14),KwExposing(3:15-3:23),OpenSquare(3:24-3:25),UpperIdent(3:25-3:29),CloseSquare(3:29-3:30),
UpperIdent(5:1-5:6),NoSpaceOpenRound(5:6-5:7),LowerIdent(5:7-5:8),Comma(5:8-5:9),Underscore(5:10-5:11),CloseRound(5:11-5:12),OpAssign(5:13-5:14),UpperIdent(5:15-5:19),NoSpaceOpenRound(5:19-5:20),Int(5:20-5:21),Comma(5:21-5:22),Int(5:23-5:24),CloseRound(5:24-5:25),
UpperIdent(6:1-6:6),NoSpaceOpenRound(6:6-6:7),Underscore(6:7-6:8),Comma(6:8-6:9),LowerIdent(6:10-6:11),CloseRound(6:11-6:12),OpAssign(6:13-6:14),UpperIdent(6:15-6:19),NoSpaceOpenRound(6:19-6:20),Int(6:20-6:21),Comma(6:21-6:22),Int(6:23-6:24),CloseRound(6:24-6:25),
UpperIdent(7:1-7:6),NoSpaceOpenRound(7:6-7:7),Underscore(7:7-7:8),Comma(7:8-7:9),Underscore(7:10-7:11),CloseRound(7:11-7:12),OpAssign(7:13-7:14),UpperIdent(7:15-7:19),NoSpaceOpenRound(7:19-7:20),Int(7:20-7:21),Comma(7:21-7:22),Int(7:23-7:24),CloseRound(7:24-7:25),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.25
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-import @3.1-3.30 (raw "Module")
			(exposing
				(exposed-upper-ident @3.25-3.29 (text "Pair"))))
		(s-malformed @5.13-5.14 (tag "expected_colon_after_type_annotation"))
		(s-malformed @6.1-6.6 (tag "expected_colon_after_type_annotation"))
		(s-malformed @6.6-6.7 (tag "statement_unexpected_token"))
		(s-malformed @6.7-6.8 (tag "statement_unexpected_token"))
		(s-malformed @6.8-6.9 (tag "statement_unexpected_token"))
		(s-malformed @6.10-6.11 (tag "statement_unexpected_token"))
		(s-malformed @6.11-6.12 (tag "statement_unexpected_token"))
		(s-malformed @6.13-6.14 (tag "statement_unexpected_token"))
		(s-malformed @7.1-7.6 (tag "expected_colon_after_type_annotation"))
		(s-malformed @7.6-7.7 (tag "statement_unexpected_token"))
		(s-malformed @7.7-7.8 (tag "statement_unexpected_token"))
		(s-malformed @7.8-7.9 (tag "statement_unexpected_token"))
		(s-malformed @7.10-7.11 (tag "statement_unexpected_token"))
		(s-malformed @7.11-7.12 (tag "statement_unexpected_token"))
		(s-malformed @7.13-7.14 (tag "statement_unexpected_token"))
		(s-malformed @1.1-1.1 (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
module []

import Module exposing [Pair]



~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.30 (ident "Pair"))
		(e-lookup-external @3.1-3.30
			(module-idx "0")
			(target-node-idx "0")))
	(s-import @3.1-3.30 (module "Module")
		(exposes
			(exposed (name "Pair") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.30 (type "Error")))
	(expressions
		(expr @3.1-3.30 (type "Error"))))
~~~
