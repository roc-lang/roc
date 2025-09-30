# META
~~~ini
description=underscore_in_assignment_pattern
type=file
~~~
# SOURCE
~~~roc
import Module exposing [Pair]

Pair1(x, _) = Pair(0, 1)
Pair2(_, y) = Pair(0, 1)
Pair3(_, _) = Pair(0, 1)
~~~
# EXPECTED
PARSE ERROR - underscore_type_decl.md:3:13:3:14
PARSE ERROR - underscore_type_decl.md:3:20:3:21
PARSE ERROR - underscore_type_decl.md:3:23:3:24
PARSE ERROR - underscore_type_decl.md:4:1:4:6
PARSE ERROR - underscore_type_decl.md:4:6:4:7
PARSE ERROR - underscore_type_decl.md:4:7:4:8
PARSE ERROR - underscore_type_decl.md:4:8:4:9
PARSE ERROR - underscore_type_decl.md:4:10:4:11
PARSE ERROR - underscore_type_decl.md:4:11:4:12
PARSE ERROR - underscore_type_decl.md:4:13:4:14
PARSE ERROR - underscore_type_decl.md:4:20:4:21
PARSE ERROR - underscore_type_decl.md:4:23:4:24
PARSE ERROR - underscore_type_decl.md:5:1:5:6
PARSE ERROR - underscore_type_decl.md:5:6:5:7
PARSE ERROR - underscore_type_decl.md:5:7:5:8
PARSE ERROR - underscore_type_decl.md:5:8:5:9
PARSE ERROR - underscore_type_decl.md:5:10:5:11
PARSE ERROR - underscore_type_decl.md:5:11:5:12
PARSE ERROR - underscore_type_decl.md:5:13:5:14
PARSE ERROR - underscore_type_decl.md:5:20:5:21
PARSE ERROR - underscore_type_decl.md:5:23:5:24
PARSE ERROR - underscore_type_decl.md:6:1:6:1
MISSING MAIN! FUNCTION - underscore_type_decl.md:1:1:5:25
MODULE NOT FOUND - underscore_type_decl.md:1:1:1:30
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

**underscore_type_decl.md:3:13:3:14:**
```roc
Pair1(x, _) = Pair(0, 1)
```
            ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:3:20:3:21:**
```roc
Pair1(x, _) = Pair(0, 1)
```
                   ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:3:23:3:24:**
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

**underscore_type_decl.md:4:1:4:6:**
```roc
Pair2(_, y) = Pair(0, 1)
```
^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:4:6:4:7:**
```roc
Pair2(_, y) = Pair(0, 1)
```
     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:4:7:4:8:**
```roc
Pair2(_, y) = Pair(0, 1)
```
      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:4:8:4:9:**
```roc
Pair2(_, y) = Pair(0, 1)
```
       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:4:10:4:11:**
```roc
Pair2(_, y) = Pair(0, 1)
```
         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:4:11:4:12:**
```roc
Pair2(_, y) = Pair(0, 1)
```
          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:4:13:4:14:**
```roc
Pair2(_, y) = Pair(0, 1)
```
            ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:4:20:4:21:**
```roc
Pair2(_, y) = Pair(0, 1)
```
                   ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:4:23:4:24:**
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

**underscore_type_decl.md:5:1:5:6:**
```roc
Pair3(_, _) = Pair(0, 1)
```
^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:6:5:7:**
```roc
Pair3(_, _) = Pair(0, 1)
```
     ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:7:5:8:**
```roc
Pair3(_, _) = Pair(0, 1)
```
      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:8:5:9:**
```roc
Pair3(_, _) = Pair(0, 1)
```
       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:10:5:11:**
```roc
Pair3(_, _) = Pair(0, 1)
```
         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:11:5:12:**
```roc
Pair3(_, _) = Pair(0, 1)
```
          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:13:5:14:**
```roc
Pair3(_, _) = Pair(0, 1)
```
            ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:20:5:21:**
```roc
Pair3(_, _) = Pair(0, 1)
```
                   ^


**PARSE ERROR**
A parsing error occurred: `invalid_type_arg`
This is an unexpected parsing error. Please check your syntax.

**underscore_type_decl.md:5:23:5:24:**
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

**underscore_type_decl.md:6:1:6:1:**
```roc

```
^


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**underscore_type_decl.md:1:1:5:25:**
```roc
import Module exposing [Pair]

Pair1(x, _) = Pair(0, 1)
Pair2(_, y) = Pair(0, 1)
Pair3(_, _) = Pair(0, 1)
```


**MODULE NOT FOUND**
The module `Module` was not found in this Roc project.

You're attempting to use this module here:
**underscore_type_decl.md:1:1:1:30:**
```roc
import Module exposing [Pair]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),UpperIdent(1:8-1:14),KwExposing(1:15-1:23),OpenSquare(1:24-1:25),UpperIdent(1:25-1:29),CloseSquare(1:29-1:30),
UpperIdent(3:1-3:6),NoSpaceOpenRound(3:6-3:7),LowerIdent(3:7-3:8),Comma(3:8-3:9),Underscore(3:10-3:11),CloseRound(3:11-3:12),OpAssign(3:13-3:14),UpperIdent(3:15-3:19),NoSpaceOpenRound(3:19-3:20),Int(3:20-3:21),Comma(3:21-3:22),Int(3:23-3:24),CloseRound(3:24-3:25),
UpperIdent(4:1-4:6),NoSpaceOpenRound(4:6-4:7),Underscore(4:7-4:8),Comma(4:8-4:9),LowerIdent(4:10-4:11),CloseRound(4:11-4:12),OpAssign(4:13-4:14),UpperIdent(4:15-4:19),NoSpaceOpenRound(4:19-4:20),Int(4:20-4:21),Comma(4:21-4:22),Int(4:23-4:24),CloseRound(4:24-4:25),
UpperIdent(5:1-5:6),NoSpaceOpenRound(5:6-5:7),Underscore(5:7-5:8),Comma(5:8-5:9),Underscore(5:10-5:11),CloseRound(5:11-5:12),OpAssign(5:13-5:14),UpperIdent(5:15-5:19),NoSpaceOpenRound(5:19-5:20),Int(5:20-5:21),Comma(5:21-5:22),Int(5:23-5:24),CloseRound(5:24-5:25),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.25
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.30 (raw "Module")
			(exposing
				(exposed-upper-ident @1.25-1.29 (text "Pair"))))
		(s-malformed @3.13-3.14 (tag "expected_colon_after_type_annotation"))
		(s-malformed @4.1-4.6 (tag "expected_colon_after_type_annotation"))
		(s-malformed @4.6-4.7 (tag "statement_unexpected_token"))
		(s-malformed @4.7-4.8 (tag "statement_unexpected_token"))
		(s-malformed @4.8-4.9 (tag "statement_unexpected_token"))
		(s-malformed @4.10-4.11 (tag "statement_unexpected_token"))
		(s-malformed @4.11-4.12 (tag "statement_unexpected_token"))
		(s-malformed @4.13-4.14 (tag "statement_unexpected_token"))
		(s-malformed @5.1-5.6 (tag "expected_colon_after_type_annotation"))
		(s-malformed @5.6-5.7 (tag "statement_unexpected_token"))
		(s-malformed @5.7-5.8 (tag "statement_unexpected_token"))
		(s-malformed @5.8-5.9 (tag "statement_unexpected_token"))
		(s-malformed @5.10-5.11 (tag "statement_unexpected_token"))
		(s-malformed @5.11-5.12 (tag "statement_unexpected_token"))
		(s-malformed @5.13-5.14 (tag "statement_unexpected_token"))
		(s-malformed @1.1-1.1 (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
import Module exposing [Pair]



~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import @1.1-1.30 (module "Module")
		(exposes
			(exposed (name "Pair") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
